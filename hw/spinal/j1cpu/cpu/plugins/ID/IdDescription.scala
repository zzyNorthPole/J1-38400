package j1cpu.cpu.plugins.ID

import j1cpu.cpu.{J1cpu, J1cpuConfig}
import j1cpu.cpu.vexriscv.Plugin
import spinal.core._

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

      decoder.io.inst := input(INST)
      for (i <- 0 until 2) {
        regFile.io.rPorts.addr(i) := decoder.io.rPorts.addr(i)
      }
      for (i <- 0 until 2) {
        byPassNetwork.io.tValid(i) := decoder.io.rPorts.en(i)
        byPassNetwork.io.tAddr(i) := decoder.io.rPorts.addr(i)
        byPassNetwork.io.tDin(i) := regFile.io.rPorts.dout(i)
      }

      insert(ALU_DIN1) := decoder.io.rPorts.en(0) ? byPassNetwork.io.tDout(0) | decoder.io.immediate
      insert(ALU_DIN2) := decoder.io.rPorts.en(1) ? byPassNetwork.io.tDout(1) | decoder.io.immediate
      insert(ALU_OP) := decoder.io.aluOp

      insert(JU_OP) := decoder.io.juOp
      insert(JU_DIN1) := byPassNetwork.io.tDout(0).asSInt
      insert(JU_DIN2) := byPassNetwork.io.tDout(1).asSInt
      insert(JU_OFFSET) := decoder.io.immediate
      // temporary set never jump as predict pc
      insert(PREDICT_PC) := input(PC) + 8

      insert(AGU_DIN1) := byPassNetwork.io.tDout(0)
      insert(AGU_DIN2) := decoder.io.immediate
      insert(MEM_EN) := decoder.io.memEn
      insert(MEM_W) := decoder.io.memW
      insert(MEM_WE) := decoder.io.memWe
      insert(MEM_DIN) := byPassNetwork.io.tDout(1)
      insert(ICACHE_OP_EN) := decoder.io.iCacheOpEn
      insert(DCACHE_OP_EN) := decoder.io.dCacheOpEn
      insert(CACHE_OP) := decoder.io.cacheOp

      insert(WB_EN) := decoder.io.wbEn
      insert(WB_REG) := decoder.io.wbReg
      insert(WB_SRC) := decoder.io.wbSrc
    }
  }
}
