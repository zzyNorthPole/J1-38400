package j1cpu.cpu.vexriscv

import spinal.core._
import spinal.lib._

import scala.collection.mutable

// use a Stageable to record a signal in pipeline
// a signal insert from a certain plugin, input into pipeline and output on posedge clk
class Stageable[T <: Data](dataType: => T) extends HardType[T](dataType) with Nameable {
  setWeakName(this.getClass.getSimpleName.replace("$", ""))
}

// use Stage to record a stage, such as IF ID EX MEM WB
class Stage() extends Area {
  def gen[T](that: => T): T = {
    // Get the head of the current component symbols tree (AST in other words)
    val body = Component.current.dslBody
    // Now all access to the SpinalHDL API will be append to it (instead of the current context)
    val ctx = body.push()
    // Empty the symbole tree (but keep a reference to the old content)
    val swapContext = body.swap()
    // Execute the block of code (will be added to the recently empty body)
    val ret = that
    // Restore the original context in which this function was called
    ctx.restore()
    // append the original symbols tree to the modified body
    swapContext.appendBack()
    return ret
  }

  // inserts records signals' initial value
  val inserts = mutable.LinkedHashMap[Stageable[Data], Data]()
  def insert[T <: Data](key: Stageable[T]): T = {
    val keyMap = key.asInstanceOf[Stageable[Data]]
    inserts.getOrElseUpdate(
      keyMap,
      gen(key())
    ).asInstanceOf[T]
  }

  // asInstanceOf: type casting

  // inputs records pipeline's input
  val inputs = mutable.LinkedHashMap[Stageable[Data], Data]()
  // inputsDefault init inputs in order to solve LATCH DETECTED problem
  val inputsDefault = mutable.LinkedHashMap[Stageable[Data], Data]()
  def input[T <: Data](key: Stageable[T]): T = {
    val keyMap = key.asInstanceOf[Stageable[Data]]
    inputs.getOrElseUpdate(
      keyMap,
      gen {
        val input, inputDefault = key()
        inputsDefault(keyMap) = inputDefault
        input := inputDefault
        return input
      }
    ).asInstanceOf[T]
  }

  // outputs records pipeline's output
  val outputs = mutable.LinkedHashMap[Stageable[Data], Data]()
  // outputsDefault init inputs in order to solve LATCH DETECTED problem
  val outputsDefault = mutable.LinkedHashMap[Stageable[Data], Data]()
  def output[T <: Data](key: Stageable[T]): T = {
    val keyMap = key.asInstanceOf[Stageable[Data]]
    outputs.getOrElseUpdate(
      keyMap,
      gen {
        val output, outputDefault = key()
        outputsDefault(keyMap) = outputDefault
        output := outputDefault
        return output
      }
    ).asInstanceOf[T]
  }

  val arbitration = new Area {
    // stop the inst by itself
    val stopCur = False
    // stop the inst by other inst
    val stopByOther = False
    // remove current inst
    val removeCur = False
    // flush the current inst and above inst
    val flushCur = False
    // flush above inst
    val flushNext = False
    // current pipeline is valid
    val isValid = Bool
    // stuck or not
    val isStuck = Bool
    // stuck by others or not
    val isStuckByOthers = Bool

    val isRemoved = removeCur
    // flush current inst or not
    val isFlushed = Bool
    //
    val isMoving = Bool
    //
    val isFiring = Bool
  }
}