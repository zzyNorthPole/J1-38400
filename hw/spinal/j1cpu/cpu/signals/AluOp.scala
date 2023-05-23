package j1cpu.cpu.signals

import spinal.core.SpinalEnum

object AluOp extends SpinalEnum {
  val ADD = newElement()
  val SUB = newElement()
  val SLT = newElement()
  val SLTU = newElement()
  val AND = newElement()
  val NOR = newElement()
  val OR = newElement()
  val XOR = newElement()
  val SLL = newElement()
  val SRL = newElement()
  val SRA = newElement()
  val LUI = newElement()
  val CLO = newElement()
  val CLZ = newElement()
}
