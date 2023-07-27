package j1cpu.cpu.signals

import spinal.core._

object TuOp extends SpinalEnum {
  val TEQ = newElement()
  val TNE = newElement()
  val TGE = newElement()
  val TGEU = newElement()
  val TLT = newElement()
  val TLTU = newElement()
}
