package j1cpu.cpu.signals

import spinal.core.SpinalEnum

object TlbOp extends SpinalEnum {
  val TLBP = newElement()
  val TLBR = newElement()
  val TLBWI = newElement()
  val TLBWR = newElement()
}
