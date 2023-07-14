package j1cpu.cpu.plugins.EX

import j1cpu.cpu.J1cpu
import j1cpu.cpu.plugins.ID.IdDescription
import j1cpu.cpu.plugins.IF.IfDescription
import j1cpu.cpu.signals.WbSrc
import j1cpu.cpu.vexriscv.Plugin
import spinal.core._

class ExDescription extends Plugin[J1cpu] {
  override def build(pipeline: J1cpu): Unit = {
    import pipeline._
    import pipeline.signal._

    EX plug new Area {
      import EX._
      pipelineSignal.stall := False

      val alu = new Alu()
      alu.io.din1 := input(ALU_DIN1)
      alu.io.din2 := input(ALU_DIN2)
      alu.io.aluOp := input(ALU_OP)
      insert(ALU_OVERFLOW) := alu.io.aluOverflow
      insert(ALU_RESULT) := alu.io.dout

      val ju = new Ju()
      ju.io.juOp := input(JU_OP)
      ju.io.din1 := input(JU_DIN1)
      ju.io.din2 := input(JU_DIN2)
      ju.io.pc := input(PC)
      ju.io.offset := input(JU_OFFSET)
      ju.io.predictPc := input(PREDICT_PC)
      insert(JU_LINK) := ju.io.link
      pipelineSignal.flush := ju.io.flush
      service[IfDescription].pcManager.io.isBranchPredictErrorFlushed := ju.io.flush
      service[IfDescription].pcManager.io.branchPredictCorrectAddr := ju.io.correctPc

      val agu = new Agu()
      agu.io.din1 := input(AGU_DIN1)
      agu.io.din2 := input(AGU_DIN2)
      insert(MEM_ADDRESS) := agu.io.addr

      service[IdDescription].byPassNetwork.io.sValid(0) := input(WB_EN)
      service[IdDescription].byPassNetwork.io.sReady(0) := !input(MEM_EN)
      service[IdDescription].byPassNetwork.io.sAddr(0) := input(WB_REG)
      service[IdDescription].byPassNetwork.io.sDin(0) := input(WB_SRC).mux(
        WbSrc.Alu() -> alu.io.dout,
        WbSrc.Ju() -> ju.io.link,
        default -> U(0, 32 bits)
      )
    }
  }
}
