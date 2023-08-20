package j1cpu.cpu.plugins.EX

import j1cpu.cpu.J1cpu
import j1cpu.cpu.plugins.ID.IdDescription
import j1cpu.cpu.plugins.IF.IfDescription
import j1cpu.cpu.plugins.MEM.MemDescription
import j1cpu.cpu.signals.{Exception, WbSrc}
import j1cpu.cpu.vexriscv.Plugin
import spinal.core._
import spinal.lib.PriorityMux

class ExDescription extends Plugin[J1cpu] {
  override def build(pipeline: J1cpu): Unit = {
    import pipeline._
    import pipeline.signal._

    EX plug new Area {
      import EX._

      val alu = new Alu()
      alu.io.en := pipelineSignal.isValid && input(ALU_EN)
      alu.io.aluOp := input(ALU_OP)
      alu.io.din1 := input(ALU_DIN1)
      alu.io.din2 := input(ALU_DIN2)
      output(WB_EN) := input(WB_EN) && !alu.io.movFail
      insert(ALU_RESULT) := alu.io.dout

      val mdu = new Mdu()
      mdu.io.flush := pipelineSignal.isFlushed
      mdu.io.en := pipelineSignal.isValid && !output(EX_EN) && input(MDU_EN)
      mdu.io.mduOp := input(MDU_OP)
      mdu.io.din1 := input(MDU_DIN1)
      mdu.io.din2 := input(MDU_DIN2)
      insert(MDU_RESULT) := mdu.io.dout
      pipelineSignal.stall := !mdu.io.ready

      val ju = new Ju()
      ju.io.en := pipelineSignal.isValid && input(JU_EN)
      ju.io.juOp := input(JU_OP)
      ju.io.din1 := input(JU_DIN1)
      ju.io.din2 := input(JU_DIN2)
      ju.io.pc := input(PC)
      ju.io.offset := input(JU_OFFSET)
      ju.io.predictPc := input(PREDICT_PC)
      insert(JU_LINK) := ju.io.link
      service[IfDescription].bpu.io.calibrate.en := pipelineSignal.isValid && input(JU_EN)
      service[IfDescription].bpu.io.calibrate.pc := input(PC)
      service[IfDescription].bpu.io.calibrate.branchTarget := ju.io.jumpPc
//      ju.io.bhr.en := input(BPU_HIT)
      ju.io.bhr.din := input(BHR)
      service[IfDescription].bpu.io.calibrate.branchHistoryRegister := ju.io.bhr.dout
      // using for delay slot
      when(ID.pipelineSignal.isValid) {
        IF2.pipelineSignal.flush := ju.io.flush
      }.otherwise {
        IF1.pipelineSignal.flush := ju.io.flush
      }
      service[IfDescription].pcManager.io.isBranchPredictErrorFlushed := pipelineSignal.isValid && ju.io.flush
      service[IfDescription].pcManager.io.branchPredictCorrectAddr := ju.io.correctPc
      val delaySlotNextPc = RegInit(U(0, 32 bits))
      when(pipelineSignal.isFlushed) {
        delaySlotNextPc := U(0, 32 bits)
      }.elsewhen(pipelineSignal.isValid && pipelineSignal.isFiring && input(JU_EN)) {
        delaySlotNextPc := ju.io.correctPc
      }.elsewhen(input(DELAY_SLOT) && pipelineSignal.isFiring) {
        delaySlotNextPc := U(0, 32 bits)
      }
      insert(DELAY_SLOT_NEXT_PC) := delaySlotNextPc

      val agu = new Agu()
      agu.io.en := pipelineSignal.isValid && input(MEM_EN)
      agu.io.memW := input(MEM_W)
      agu.io.memOp := input(MEM_OP)
      agu.io.din1 := input(AGU_DIN1)
      agu.io.din2 := input(AGU_DIN2)
      insert(MEM_ADDRESS) := agu.io.addr
      insert(MEM_WE) := agu.io.memWe
      agu.io.din := input(MEM_DIN)
      output(MEM_DIN) := agu.io.dout

      val tu = new Tu()
      tu.io.en := pipelineSignal.isValid && input(TU_EN)
      tu.io.tuOp := input(TU_OP)
      tu.io.din1 := input(TU_DIN1)
      tu.io.din2 := input(TU_DIN2)

      val interrupt = service[MemDescription].cp0.io.interrupt
      val overflow = alu.io.overflow
      val dAdEL = agu.io.addressErrorLoad
      val dAdES = agu.io.addressErrorStore
      val trap = tu.io.trap
      output(EX_EN) := interrupt || input(EX_EN) || overflow || dAdEL || dAdES || trap
      output(EX_OP) := PriorityMux(
        Vec(
          interrupt,
          input(EX_EN),
          overflow,
          trap,
          dAdEL,
          dAdES
        ),
        Vec(
          Exception.INT(),
          input(EX_OP),
          Exception.OV(),
          Exception.TR(),
          Exception.ADEL(),
          Exception.ADES()
        )
      )
      output(EX_BAD_ADDR) := input(EX_EN) ? input(EX_BAD_ADDR) | input(MEM_ADDRESS)

      service[IdDescription].byPassNetwork.io.sValid(0) := pipelineSignal.isValid && input(WB_EN) && !alu.io.movFail
      service[IdDescription].byPassNetwork.io.sReady(0) := !input(MEM_EN) && !input(CP0_EN)
      service[IdDescription].byPassNetwork.io.sAddr(0) := input(WB_REG)
      service[IdDescription].byPassNetwork.io.sDin(0) := input(WB_SRC).mux(
        WbSrc.Alu() -> alu.io.dout,
        WbSrc.Mdu() -> mdu.io.dout,
        WbSrc.Ju() -> ju.io.link,
        default -> U(0, 32 bits)
      )
    }
  }
}
