package j1cpu.cpu.plugins.IF

import j1cpu.cpu.{J1cpu, J1cpuConfig}
import j1cpu.cpu.vexriscv.Plugin
import spinal.core._
import spinal.lib._

class IfDescription(config: J1cpuConfig) extends Plugin[J1cpu] {
  val pcManager = new PcManager()
  val iCache = new ICache(config.cacheConfig, config.axiConfig, config.sim)
  override def build(pipeline: J1cpu): Unit = {
    import pipeline._
    import pipeline.signal._

    IF1 plug new Area {
      import IF1._
      pipelineSignal.flush := False
      pipelineSignal.stall := False

      val curPc = pcManager.io.pc
      insert(PC) := curPc
      pcManager.io.isStalled := pipelineSignal.isStalled

      iCache.io.flush := pipelineSignal.isFlushed
      iCache.io.en := pipelineSignal.isValid && !pipelineSignal.isStalled
      iCache.io.we := B(0, 4 bits)
      iCache.io.addr := curPc
    }

    IF2 plug new Area {
      import IF2._
      pipelineSignal.flush := False
      pipelineSignal.stall := !iCache.io.ready

      val instReg = RegInit(U(0, 32 bits))
      val instRegValid = RegInit(False)
      when(iCache.io.valid) {
        instRegValid := True
        instReg := iCache.io.dout
      }
      when(!pipelineSignal.isStalled) {
        instRegValid := False
      }
      insert(INST) := (instRegValid ? instReg | iCache.io.dout).asBits
    }
  }
}
