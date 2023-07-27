package j1cpu.cpu.signals

import spinal.core.SpinalEnum

object MemOp extends SpinalEnum {
  val B = newElement()
  val H = newElement()
  val WL = newElement()
  val W = newElement()
  val BU = newElement()
  val HU = newElement()
  val WR = newElement()
}
