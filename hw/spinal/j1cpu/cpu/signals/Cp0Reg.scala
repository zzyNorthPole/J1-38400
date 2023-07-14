package j1cpu.cpu.signals

import spinal.core.SpinalEnum

object Cp0Reg extends SpinalEnum {
  val INDEX = newElement()
  val RANDOM = newElement()
  val ENTRYLO0 = newElement()
  val ENTRYLO1 = newElement()
  val CONTEXT = newElement()
  val PAGEMASK = newElement()
  val WIRED = newElement()
  val ENTRYHI = newElement()
  val STATUS = newElement()
  val CAUSE = newElement()
  val EPC = newElement()
  val EBASE = newElement()
  val BADVADDR = newElement()
  val COUNT = newElement()
  val CONFIG = newElement()
}
