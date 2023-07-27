package j1cpu.cpu.vexriscv

import spinal.core._
import spinal.lib._
import spinal.lib.pipeline.Stageable

import scala.collection.mutable


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

  val inserts = mutable.LinkedHashMap[Signal[Data], Data]()
  def insert[T <: Data](key: Signal[T]): T = {
    inserts.getOrElseUpdate(
      key.asInstanceOf[Signal[Data]],
      gen(key())
    ).asInstanceOf[T]
  }

  val inputs = mutable.LinkedHashMap[Signal[Data], Data]()
  def input[T <: Data](key: Signal[T]): T = {
    inputs.getOrElseUpdate(
      key.asInstanceOf[Signal[Data]],
      gen(key())
    ).asInstanceOf[T]
  }

  val outputs = mutable.LinkedHashMap[Signal[Data], Data]()
  def output[T <: Data](key: Signal[T]): T = {
    outputs.getOrElseUpdate(
      key.asInstanceOf[Signal[Data]],
      gen(key())
    ).asInstanceOf[T]
  }

  val pipelineSignal = new Area {
    val flush = Bool
    val stall = Bool
    val isFlushed = Bool
    val isStalled = Bool
    val isValid = Bool
    val isMoving = Bool
    val isFiring = Bool
  }
}