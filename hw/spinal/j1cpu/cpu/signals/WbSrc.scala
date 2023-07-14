package j1cpu.cpu.signals

import spinal.core.SpinalEnum

object WbSrc extends SpinalEnum {
  val Alu = newElement()
  val Mdu = newElement()
  val Ju = newElement()
  val DCache = newElement()
  val Cp0 = newElement()
}
