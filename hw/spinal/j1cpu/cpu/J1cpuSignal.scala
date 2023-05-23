package j1cpu.cpu

import spinal.core._
import spinal.lib._

import j1cpu.cpu.vexriscv._
import j1cpu.cpu.signals._

class J1cpuSignal() {
  object ALU_OP extends Signal(AluOp())
  object ALU_SRC1 extends Signal(UInt(32 bits))
  object ALU_SRC2 extends Signal(UInt(32 bits))

  object ALU_OVERFLOW extends Signal(Bool())
  object ALU_RESULT extends Signal(UInt(32 bits))
}
