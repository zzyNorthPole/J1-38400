package j1cpu.cpu.vexriscv

import spinal.core._
import spinal.lib._

import scala.collection.mutable
import scala.reflect.ClassTag

trait Pipeline {
  type T <: Pipeline

  // plugin related
  val plugins = mutable.ArrayBuffer[Plugin[T]]()

  def service[T](implicit tag: ClassTag[T]) = {
    val clazz = tag.runtimeClass
    val filtered = plugins.filter(tmp => tmp.getClass == clazz)
    filtered.head.asInstanceOf[T]
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

  def nxtStage(stage: Stage) = stages(stageId(stage) + 1)

  def newStage(): Stage = {
    val stage = new Stage()
    stages += stage
    stage
  }

  def buildPipelineSignal() = {
    for (i <- 0 until stages.length; stage = stages(i)) {
      // isFlushed && flush => isFlushed (<-)
      if (i + 1 == stages.length)
        stage.pipelineSignal.isFlushed := stage.pipelineSignal.flush
      else
        stage.pipelineSignal.isFlushed := stage.pipelineSignal.flush || nxtStage(stage).pipelineSignal.isFlushed
      // isStalled && stall => isStalled (<-)
      if (i + 1 == stages.length)
        stage.pipelineSignal.isStalled := stage.pipelineSignal.stall
      else
        stage.pipelineSignal.isStalled := stage.pipelineSignal.stall || nxtStage(stage).pipelineSignal.isStalled
      // isStalled && isFlushed => isMoving (<-)
      stage.pipelineSignal.isMoving := !stage.pipelineSignal.isStalled && !stage.pipelineSignal.isFlushed
      // isMoving && isValid => isFiring (<-)
      stage.pipelineSignal.isFiring := stage.pipelineSignal.isValid && stage.pipelineSignal.isMoving
      // reg!!! isStuck && removeCur => isValid (->)
      if (i > 0) {
        stage.pipelineSignal.isValid.setAsReg() init(False)
        // when current inst is flushed, it will jump to the second position and be set false
        // when current inst is stuck, it won't be changed
        // when current inst isn't stuck:
        // if previous inst is flushed, it will jump to the second position and be set false
        // if previous inst is stuck, it will jump to the second position and be set false
        // if previous inst isn't stuck, it will be set to the previous value
        // What an excellent design!
        when (!preStage(stage).pipelineSignal.isStalled && !preStage(stage).pipelineSignal.isFlushed) {
          stage.pipelineSignal.isValid := preStage(stage).pipelineSignal.isValid
        } .elsewhen (!stage.pipelineSignal.isStalled || stage.pipelineSignal.isFlushed) {
          stage.pipelineSignal.isValid := False
        }
      }
      else {
        stage.pipelineSignal.isValid.setAsReg() init(True)
        // if current inst isn't flushed:
        // if current inst isn't stuck: it will be set true
        // if current inst is stuck: it will not change
        // if current inst is flushed:
        // if current inst isn't stuck, it will be set false
        // if current inst is stuck, it will be set true
        when (stage.pipelineSignal.isFlushed) {
          stage.pipelineSignal.isValid := False
        }
        when (!stage.pipelineSignal.isStalled) {
          stage.pipelineSignal.isValid := True
        }
      }
    }
  }

  def buildSignal() = {
    System.out.println("Begin")
    class SignalDescription {
      var insertId = Int.MaxValue
      var lastInputId = Int.MinValue
      var lastOutputId = Int.MinValue

      def setInsertId(stageId: Int) = {
        insertId = stageId
      }

      def updInputId(stageId: Int) = {
        lastInputId = Math.max(stageId, lastInputId)
      }

      def updOutputId(stageId: Int) = {
        lastOutputId = Math.max(stageId, lastOutputId)
      }
    }

    val inputOutputInfo = mutable.LinkedHashMap[Signal[Data], SignalDescription]()
    val insertSignal = mutable.Set[Signal[Data]]()
    // make INSERT and confirm the passing stage of each signal
    for (i <- 0 until stages.length; stage = stages(i)) {
      stage.inserts.keysIterator.foreach {
        signal => {
          inputOutputInfo.getOrElseUpdate(
            signal,
            new SignalDescription
          ).setInsertId(i)
          insertSignal += signal
        }
      }
    }

    for (i <- 0 until stages.length; stage = stages(i)) {
      stage.inputs.keysIterator.foreach(
        signal => {
          inputOutputInfo.getOrElseUpdate(
            signal,
            new SignalDescription
          ).updInputId(i)
        }
      )
      stage.outputs.keysIterator.foreach(
        signal => {
          inputOutputInfo.getOrElseUpdate(
            signal,
            new SignalDescription
          ).updOutputId(i)
        }
      )
    }

    for ((signal, info) <- inputOutputInfo) {
      if (info.lastOutputId > info.lastInputId) info.lastInputId = info.lastOutputId
      else if (info.lastOutputId < info.lastInputId) info.lastOutputId = info.lastInputId - 1
    }

    for ((signal, info) <- inputOutputInfo) {
      // connect inputs -> outputs
      for (i <- info.insertId to info.lastOutputId; stage = stages(i)) {
        stage.output(signal)
        val output = stage.outputs.getOrElse(signal, null)
        output.setName(s"${stage.getName()}_${signal.getName()}_out")
        if (output != null) {
          val input = stage.input(signal)
          input.setName(s"${stage.getName()}_${signal.getName()}_in")
          output := input
        }
      }
      // connect outputs -> inputs
      for (i <- info.insertId to info.lastInputId; stage = stages(i)) {
        stage.input(signal)
        val input = stage.inputs.getOrElse(signal, null)
        input.setName(s"${stage.getName()}_${signal.getName()}_in")
        if (input != null) {
          if (i == info.insertId) {
            val insert = stage.inserts(signal)
            insert.setName(s"${stage.getName()}_${signal.getName()}")
            input := insert
          }
          else {
            val pOutput = preStage(stage).output(signal)
            pOutput.setName(s"${preStage(stage).getName()}_${signal.getName()}_out")
            input := RegNextWhen(
              pOutput,
              // when current inst is flushed, valid will be set false, output -> input
              // when current inst is stuck, it won't be changed, won't be changed
              // when current inst isn't stuck:
              // if previous inst is flushed, valid will be set false, output -> input
              // if previous inst is stuck, valid will be set false, output -> input
              // if previous inst isn't stuck, valid will be set to the previous value, output -> input (change and valid)
              !stage.pipelineSignal.isStalled
            ).setName(s"${preStage(stage).getName()}_${stage.getName()}_${signal.getName()}_reg")
          }
        }
      }
    }
  }

  def build() = {
    buildPlugins()
    buildSignal()
    buildPipelineSignal()
  }

  Component.current.addPrePopTask(() => build())
}