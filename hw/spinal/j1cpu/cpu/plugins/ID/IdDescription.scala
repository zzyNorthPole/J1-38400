package j1cpu.cpu.plugins.ID

import j1cpu.cpu.plugins.MEM.MemDescription
import j1cpu.cpu.signals.Exception
import j1cpu.cpu.{J1cpu, J1cpuConfig}
import j1cpu.cpu.vexriscv.Plugin
import spinal.core._
import spinal.lib.PriorityMux

class IdDescription(config: J1cpuConfig) extends Plugin[J1cpu] {
  val regFile = new RegFile(config.regFillConfig)
  val decoder = new Decoder()
  val byPassNetwork = new ByPassNetwork(config.byPassConfig)
  override def build(pipeline: J1cpu): Unit = {
    import pipeline._
    import pipeline.signal._

    ID plug new Area {
      import ID._
      pipelineSignal.flush := False
      pipelineSignal.stall := byPassNetwork.io.stall(0) || byPassNetwork.io.stall(1)

      decoder.io.pc := input(PC)
      decoder.io.inst := input(INST)
      for (i <- 0 until 2) {
        regFile.io.rPorts.addr(i) := decoder.io.rPorts.addr(i)
      }
      for (i <- 0 until 2) {
        byPassNetwork.io.tValid(i) := pipelineSignal.isValid && decoder.io.rPorts.en(i)
        byPassNetwork.io.tAddr(i) := decoder.io.rPorts.addr(i)
        byPassNetwork.io.tDin(i) := regFile.io.rPorts.dout(i)
      }

      insert(ALU_EN) := decoder.io.aluEn
      insert(ALU_OP) := decoder.io.aluOp
      insert(ALU_DIN1) := decoder.io.rPorts.en(0) ? byPassNetwork.io.tDout(0) | decoder.io.immediate
      insert(ALU_DIN2) := decoder.io.rPorts.en(1) ? byPassNetwork.io.tDout(1) | decoder.io.immediate

      insert(MDU_EN) := decoder.io.mduEn
      insert(MDU_OP) := decoder.io.mduOp
      insert(MDU_DIN1) := decoder.io.rPorts.en(0) ? byPassNetwork.io.tDout(0) | decoder.io.immediate
      insert(MDU_DIN2) := decoder.io.rPorts.en(1) ? byPassNetwork.io.tDout(1) | decoder.io.immediate

      insert(JU_EN) := decoder.io.juEn
      insert(JU_OP) := decoder.io.juOp
      insert(JU_DIN1) := (decoder.io.rPorts.en(0) ? byPassNetwork.io.tDout(0) | decoder.io.immediate).asSInt
      insert(JU_DIN2) := (decoder.io.rPorts.en(1) ? byPassNetwork.io.tDout(1) | decoder.io.immediate).asSInt
      insert(JU_OFFSET) := decoder.io.immediate
      // temporary set never jump as predict pc
      insert(PREDICT_PC) := input(PC) + 8
      val delaySlot = RegInit(False)
      when(pipelineSignal.isFlushed) {
        delaySlot := False
      }.elsewhen(pipelineSignal.isValid && pipelineSignal.isFiring && input(JU_EN)) {
        delaySlot := True
      }.elsewhen(delaySlot && pipelineSignal.isFiring) {
        delaySlot := False
      }
      insert(DELAY_SLOT) := delaySlot

      insert(AGU_DIN1) := byPassNetwork.io.tDout(0)
      insert(AGU_DIN2) := decoder.io.immediate
      insert(MEM_EN) := decoder.io.memEn
      insert(MEM_W) := decoder.io.memW
      insert(MEM_OP) := decoder.io.memOp
      insert(MEM_DIN) := byPassNetwork.io.tDout(1)

      insert(TLB_OP_EN) := decoder.io.tlbOpEn
      insert(TLB_OP) := decoder.io.tlbOp

      insert(ICACHE_OP_EN) := decoder.io.iCacheOpEn
      insert(DCACHE_OP_EN) := decoder.io.dCacheOpEn
      insert(CACHE_OP) := decoder.io.cacheOp

      insert(TU_EN) := decoder.io.tuEn
      insert(TU_OP) := decoder.io.tuOp
      insert(TU_DIN1) := decoder.io.rPorts.en(0) ? byPassNetwork.io.tDout(0) | decoder.io.immediate
      insert(TU_DIN2) := decoder.io.rPorts.en(1) ? byPassNetwork.io.tDout(1) | decoder.io.immediate

      insert(CP0_EN) := decoder.io.cp0En
      insert(CP0_W) := decoder.io.cp0W
      insert(CP0_REG) := decoder.io.cp0Reg
      insert(CP0_SELECT) := decoder.io.cp0Select
      insert(CP0_DIN) := byPassNetwork.io.tDout(1)

      insert(WB_EN) := decoder.io.wbEn
      insert(WB_REG) := decoder.io.wbReg
      insert(WB_SRC) := decoder.io.wbSrc

      insert(ERET) := decoder.io.eret

      val interrupt = service[MemDescription].cp0.io.interrupt
      val syscall = decoder.io.syscall
      val break = decoder.io.break
      val reserveInst = decoder.io.reserveInst
      val copUnusable = decoder.io.copUnusable
      output(EX_EN) := interrupt | input(EX_EN) | syscall | break | reserveInst | copUnusable
      output(EX_OP) := PriorityMux(
        Vec(
          interrupt,
          input(EX_EN),
          syscall,
          break,
          reserveInst,
          copUnusable
        ),
        Vec(
          Exception.INT(),
          input(EX_OP),
          Exception.SYS(),
          Exception.BP(),
          Exception.RI(),
          Exception.CPU()
        )
      )
    }
  }
}
