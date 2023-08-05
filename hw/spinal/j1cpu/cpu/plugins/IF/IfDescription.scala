package j1cpu.cpu.plugins.IF

import j1cpu.cpu.plugins.MEM.MemDescription
import j1cpu.cpu.plugins.MMU.Mmu
import j1cpu.cpu.signals.Exception
import j1cpu.cpu.{J1cpu, J1cpuConfig}
import j1cpu.cpu.vexriscv.Plugin
import spinal.core._
import spinal.lib._

class IfDescription(config: J1cpuConfig) extends Plugin[J1cpu] {
  val pcManager = new PcManager()
  val iCache = new ICache(config.iCacheConfig, config.axiConfig, config.sim)
  val iMmu = new Mmu(config.tlbConfig)
  val bpu = new Bpu(config.bpuConfig, config.sim)
  override def build(pipeline: J1cpu): Unit = {
    import pipeline._
    import pipeline.signal._

    IF1 plug new Area {
      import IF1._
      pipelineSignal.flush := False
      pipelineSignal.stall := False

      val curPc = pcManager.io.pc
      insert(PC) := curPc
      pcManager.io.isValid := pipelineSignal.isValid
      pcManager.io.isStalled := pipelineSignal.isStalled
      val iAdEL = pcManager.io.addressErrorLoad

      iMmu.io.en := pipelineSignal.isValid
      iMmu.io.w := False
      iMmu.io.virtAddr := curPc
      iMmu.io.k0Cached := service[MemDescription].cp0.io.k0Cached
      if (!config.tlbConfig.use) {
        iMmu.io.tlb.din.PFN := U(0, 20 bits)
        iMmu.io.tlb.din.C := U(0, 3 bits)
        iMmu.io.tlb.din.D := False
        iMmu.io.tlb.din.V := False
        iMmu.io.tlb.hit := True
      }
      else {
        service[MemDescription].tlb.io.queryPorts(0).din.VPN := iMmu.io.tlb.VPN
        service[MemDescription].tlb.io.queryPorts(0).din.ASID := service[MemDescription].cp0.io.tlbpDout.ASID
        iMmu.io.tlb.din := service[MemDescription].tlb.io.queryPorts(0).dout
        iMmu.io.tlb.hit := service[MemDescription].tlb.io.queryPorts(0).hit
      }
      val iRefill = iMmu.io.tlbException.refill
      val iInvalid = iMmu.io.tlbException.invalid
      val iModified = iMmu.io.tlbException.modified
      insert(EX_EN) := (iAdEL || iRefill || iInvalid || iModified)
      insert(EX_OP) := PriorityMux(
        Vec(
          iAdEL,
          iRefill,
          iInvalid,
          iModified
        ),
        Vec(
          Exception.ADEL(),
          Exception.TLBL(),
          Exception.TLBL(),
          Exception.MOD()
        )
      )
      insert(EX_TLB_HIT) := input(EX_EN) && iInvalid
      insert(EX_BAD_ADDR) := input(PC)
      insert(EX_BAD_TLB_REQUEST).VPN := iMmu.io.tlb.VPN
      insert(EX_BAD_TLB_REQUEST).ASID := service[MemDescription].cp0.io.tlbpDout.ASID

      bpu.io.predict.isStalled := pipelineSignal.isStalled
      bpu.io.predict.pc := curPc
      bpu.io.predict.en := pipelineSignal.isValid && !pipelineSignal.isStalled

      iCache.io.fetch.flush := pipelineSignal.isFlushed
      iCache.io.fetch.exception := pipelineSignal.isValid && input(EX_EN)
      iCache.io.fetch.en := pipelineSignal.isValid && !pipelineSignal.isStalled
      iCache.io.fetch.we := B(0, 4 bits)
      iCache.io.fetch.addr := iMmu.io.phyAddr
      iCache.io.fetch.cached := iMmu.io.cached
      iCache.io.fetch.correctTag := iMmu.io.phyAddr(31 downto 32 - config.iCacheConfig.tagWidth)
    }

    IF2 plug new Area {
      import IF2._
      pipelineSignal.flush := False
      pipelineSignal.stall := iCache.io.fetch.isStalled

      bpu.io.predict.exception := input(EX_EN)
      pcManager.io.isBranchPredict := pipelineSignal.isValid && !pipelineSignal.isStalled && bpu.io.predict.isBranchPredict
      pcManager.io.branchPredictAddr := bpu.io.predict.branchPredictAddr
      insert(PREDICT_PC) := bpu.io.predict.branchPredictAddr
      insert(BPU_HIT) := bpu.io.predict.hit
      insert(BHR) := bpu.io.predict.branchHistoryRegister

      val instReg = RegInit(U(0, 32 bits))
      val instRegValid = RegInit(False)
      when(iCache.io.fetch.ready) {
        instRegValid := True
        instReg := iCache.io.fetch.dout
      }
      when(!pipelineSignal.isStalled) {
        instRegValid := False
      }
      insert(INST) := (instRegValid ? instReg | iCache.io.fetch.dout).asBits
    }
  }
}
