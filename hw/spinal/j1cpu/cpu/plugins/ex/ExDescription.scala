package j1cpu.cpu.plugins.ex

import j1cpu.cpu.J1cpu
import j1cpu.cpu.vexriscv.Plugin

import spinal.core._
import spinal.lib._

class ExDescription extends Plugin[J1cpu] {
  override def build(pipeline: J1cpu): Unit = {
    import pipeline._
    import pipeline.signal._

    EX plug new Area {
      import EX._

      val alu = new Alu()
      alu.io.aluSrc1 := input(ALU_SRC1)
      alu.io.aluSrc2 := input(ALU_SRC2)
      alu.io.aluOp := input(ALU_OP)
      insert(ALU_OVERFLOW) := alu.io.aluOverflow
      insert(ALU_RESULT) := alu.io.aluResult
    }
  }
}
