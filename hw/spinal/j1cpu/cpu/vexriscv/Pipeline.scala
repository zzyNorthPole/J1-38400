package j1cpu.cpu.vexriscv

import spinal.core._
import spinal.lib._
import scala.collection.mutable

trait Pipeline {
  type T <: Pipeline

  // plugin related
  val plugins = mutable.ArrayBuffer[Plugin[T]]()

  def service[T](clazz: Class[T]): T = {
    val filtered = plugins.filter(tmp => (tmp.getClass == clazz))
    return filtered.head.asInstanceOf[T]
  }

  def buildPlugins() = {
    plugins.foreach(_.pipeline = this.asInstanceOf[T])
    plugins.foreach(_.setup(this.asInstanceOf[T]))
    plugins.foreach {
      p =>
        p.parentScope = Component.current.dslBody
        p.reflectNames()
    }
    plugins.foreach(_.build(this.asInstanceOf[T]))
  }

  // stage related

  var stages = mutable.ArrayBuffer[Stage]()

  def stageId(stage: Stage) = stages.indexOf(stage)

  def preStage(stage: Stage) = stages(stageId(stage) - 1)

  def sufStage(stage: Stage) = stages(stageId(stage) + 1)

  def newStage(): Stage = {
    val stage = new Stage()
    stages += stage
    return stage
  }

  def buildArbitration() = {
    for (i <- 0 until stages.length; stage = stages(i)) {
      // flushNext & flushCur => isFlushed (<-)
      stage.arbitration.isFlushed :=
        stages.drop(i + 1).map(_.arbitration.flushNext).orR || stages.drop(i).map(_.arbitration.flushCur).orR
      stage.arbitration.removeCur := stage.arbitration.isFlushed
      // isStuck(i+1...) & stopByOther => isStuckByOthers (<-)
      stage.arbitration.isStuckByOthers :=
        stage.arbitration.stopByOther || stages.drop(i + 1).map(_.arbitration.isStuck).orR
      // isStuckByOthers & stopCur => isStuck (<-)
      stage.arbitration.isStuck := stage.arbitration.stopCur || stage.arbitration.isStuckByOthers
      // isStuck && removeCur => isMoving (<-)
      stage.arbitration.isMoving := !stage.arbitration.isStuck && !stage.arbitration.removeCur
      // isMoving && isValid => isFiring (<-)
      stage.arbitration.isFiring := stage.arbitration.isValid && stage.arbitration.isMoving
      // reg!!! isStuck && removeCur => isValid (->)
      if (i > 0) {
        stage.arbitration.isValid.setAsReg() init(False)
        // when current inst is flushed, it will jump to the second position and be set false
        // when current inst is stuck, it won't be changed
        // when current inst isn't stuck:
        // if previous inst is flushed, it will jump to the second position and be set false
        // if previous inst is stuck, it will jump to the second position and be set false
        // if previous inst isn't stuck, it will be set to the previous value
        // What an excellent design!
        when (!preStage(stage).arbitration.isStuck && !preStage(stage).arbitration.removeCur) {
          stage.arbitration.isValid := preStage(stage).arbitration.isValid
        } .elsewhen (!stage.arbitration.isStuck || stage.arbitration.removeCur) {
          stage.arbitration.isValid := False
        }
      }
      else {
        // TODO: ensure fetch behavior
        stage.arbitration.isValid.setAsReg() init(True)
        when (stage.arbitration.removeCur) {
          stage.arbitration.isValid := False
        }
        when (!stage.arbitration.isStuck) {
          stage.arbitration.isValid := True
        }
      }
    }
  }

  def buildPipelineSignal() = {
    class KeyInfo {
      var insertStageId = Int.MaxValue
      var lastInputStageId = Int.MinValue
      var lastOutputStageId = Int.MinValue

      def setInsertStageId(stageId: Int) = {
        insertStageId = stageId
      }

      def addInputStageId(stageId: Int) = {
        require(stageId >= insertStageId)
        lastInputStageId = Math.max(lastInputStageId, stageId)
        lastOutputStageId = Math.max(lastOutputStageId, stageId - 1)
      }

      def addOutputStageId(stageId: Int) = {
        require(stageId >= insertStageId)
        lastOutputStageId = Math.max(lastOutputStageId, stageId)
        lastInputStageId = Math.max(lastInputStageId, stageId)
      }
    }

    val inputOutputKeys = mutable.LinkedHashMap[Stageable[Data], KeyInfo]()
    val insertStageable = mutable.Set[Stageable[Data]]()
    // make INSERT and confirm the passing stage of each signal
    for (i <- 0 until stages.length; stage = stages(i)) {
      stage.inserts.keysIterator.foreach(
        signal =>
          inputOutputKeys.getOrElseUpdate(signal, new KeyInfo).setInsertStageId(i)
      )
      stage.inserts.keysIterator.foreach(
        insertStageable += _
      )
    }
    // check whether some signal be ignore
    val missingInserts = mutable.Set[Stageable[Data]]()
    for (i <- 0 until stages.length; stage = stages(i)) {
      stage.inputs.keysIterator.foreach(
        key =>
          if (!insertStageable.contains(key)) missingInserts += key
      )
      stage.outputs.keysIterator.foreach(
        key =>
          if (!insertStageable.contains(key)) missingInserts += key
      )
    }
    if (missingInserts.nonEmpty) {
      throw new Exception("Missing inserts: " + missingInserts.map(_.getName()).mkString(", "))
    }

    for (i <- 0 until stages.length; stage = stages(i)) {
      stage.inputs.keysIterator.foreach(
        key =>
          inputOutputKeys.getOrElseUpdate(key, new KeyInfo).addInputStageId(i)
      )
      stage.outputs.keysIterator.foreach(
        key =>
          inputOutputKeys.getOrElseUpdate(key, new KeyInfo).addOutputStageId(i)
      )
    }

    for ((key, info) <- inputOutputKeys) {
      // connect inputs -> outputs
      for (i <- info.insertStageId to info.lastOutputStageId; stage = stages(i)) {
        stage.output(key)
        val outputDefault = stage.outputsDefault.getOrElse(key, null)
        if (outputDefault != null) outputDefault := stage.input(key)
      }
      // connect outputs -> inputs
      for (i <- info.insertStageId to info.lastInputStageId; stage = stages(i)) {
        stage.input(key)
        val inputDefault = stage.inputsDefault.getOrElse(key, null)
        if (inputDefault != null) {
          if (i == info.insertStageId) inputDefault := stage.inserts(key)
          else inputDefault := RegNextWhen(
            preStage(stage).output(key),
            // when current inst is flushed, valid will be set false, output -> input
            // when current inst is stuck, it won't be changed, won't be changed
            // when current inst isn't stuck:
            // if previous inst is flushed, valid will be set false, output -> input
            // if previous inst is stuck, valid will be set false, output -> input
            // if previous inst isn't stuck, valid will be set to the previous value, output -> input (change and valid)
            !stage.arbitration.isStuck
          )
        }
      }
    }
  }

  def build() = {
    buildPlugins()
    buildArbitration()
    buildPipelineSignal()
  }
}
