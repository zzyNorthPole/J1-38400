package j1cpu.cpu.plugins.MEM

import j1cpu.cpu.plugins.ID.IdDescription
import j1cpu.cpu.plugins.IF.IfDescription
import j1cpu.cpu.plugins.MMU.{Mmu, TLB}
import j1cpu.cpu.signals.{Exception, MemOp, TlbOp, WbSrc}
import j1cpu.cpu.{J1cpu, J1cpuConfig}
import j1cpu.cpu.vexriscv.Plugin
import spinal.core._
import spinal.lib._

class MemDescription(config: J1cpuConfig) extends Plugin[J1cpu] {
  val cp0 = new Cp0(config)
  val dCache = new DCache(config.cacheConfig, config.axiConfig, config.sim)
  val dMmu = new Mmu(config.tlbConfig)
  val dgu = new Dgu()
  val tlb = new TLB(config.tlbConfig)
  override def build(pipeline: J1cpu): Unit = {
    import pipeline._
    import pipeline.signal._

    val ByPassNetWorkService = service[IdDescription].byPassNetwork
    MEM1 plug new Area {
      import MEM1._
      pipelineSignal.flush := False
      pipelineSignal.stall := False

      dMmu.io.en := pipelineSignal.isValid && input(MEM_EN)
      dMmu.io.virtAddr := input(MEM_ADDRESS)
      dMmu.io.k0Cached := cp0.io.k0Cached
      if (!config.tlbConfig.use) {
        tlb.io.queryPorts(1).din.VPN := U(0, 20 bits)
        tlb.io.queryPorts(1).din.ASID := U(0, 8 bits)
        dMmu.io.tlb.din.PFN := U(0, 20 bits)
        dMmu.io.tlb.din.C := U(0, 3 bits)
        dMmu.io.tlb.din.D := False
        dMmu.io.tlb.din.V := False
        dMmu.io.tlb.hit := True

        // tlb operation
        cp0.io.tlbp := False
        cp0.io.tlbpHit := False
        cp0.io.tlbpDin := U(0, log2Up(config.tlbConfig.lines) bits)

        cp0.io.tlbr := False
        tlb.io.rPorts.addr := U(0, log2Up(config.tlbConfig.lines) bits)
        cp0.io.tlbrDin := tlb.io.rPorts.dout

        cp0.io.tlbw := False
        tlb.io.wPorts.en := False
        tlb.io.wPorts.addr := U(0, log2Up(config.tlbConfig.lines) bits)
        tlb.io.wPorts.din.VPN2 := U(0, 19 bits)
        tlb.io.wPorts.din.ASID := U(0, 8 bits)
        tlb.io.wPorts.din.G := False
        tlb.io.wPorts.din.PFN0 := U(0, 20 bits)
        tlb.io.wPorts.din.PFN1 := U(0, 20 bits)
        tlb.io.wPorts.din.C0 := U(0, 3 bits)
        tlb.io.wPorts.din.C1 := U(0, 3 bits)
        tlb.io.wPorts.din.D0 := False
        tlb.io.wPorts.din.D1 := False
        tlb.io.wPorts.din.V0 := False
        tlb.io.wPorts.din.V1 := False
      }
      else {
        tlb.io.queryPorts(1).din.VPN := (input(TLB_OP_EN) && (input(TLB_OP) === TlbOp.TLBP)) ? cp0.io.tlbpDout.VPN | dMmu.io.tlb.VPN
        tlb.io.queryPorts(1).din.ASID := cp0.io.tlbpDout.ASID
        dMmu.io.tlb.din := tlb.io.queryPorts(1).dout
        dMmu.io.tlb.hit := tlb.io.queryPorts(1).hit

        // tlb operation
        cp0.io.tlbp := pipelineSignal.isValid && !pipelineSignal.isStalled && input(TLB_OP_EN) && (input(TLB_OP) === TlbOp.TLBP)
        cp0.io.tlbpHit := tlb.io.queryPorts(1).hit
        cp0.io.tlbpDin := tlb.io.queryPorts(1).addr

        cp0.io.tlbr := pipelineSignal.isValid && !pipelineSignal.isStalled && input(TLB_OP_EN) && (input(TLB_OP) === TlbOp.TLBR)
        tlb.io.rPorts.addr := cp0.io.tlbrAddr
        cp0.io.tlbrDin := tlb.io.rPorts.dout

        cp0.io.tlbw := pipelineSignal.isValid && !pipelineSignal.isStalled && input(TLB_OP_EN) && (input(TLB_OP) === TlbOp.TLBWR)
        tlb.io.wPorts.en := pipelineSignal.isValid && !pipelineSignal.isStalled && input(TLB_OP_EN) && (input(TLB_OP) === TlbOp.TLBWI || input(TLB_OP) === TlbOp.TLBWR)
        tlb.io.wPorts.addr := cp0.io.tlbwAddr
        tlb.io.wPorts.din := cp0.io.tlbwDout
      }

      dCache.io.flush := pipelineSignal.isFlushed
      dCache.io.exception := output(EX_EN)
      dCache.io.en := pipelineSignal.isValid && !pipelineSignal.isStalled && input(MEM_EN) && !input(ICACHE_OP_EN)
      dCache.io.we := input(MEM_WE)
      dCache.io.addr := dMmu.io.phyAddr
      dCache.io.din := input(MEM_DIN)

      dCache.io.cached := dMmu.io.cached
      dCache.io.correctTag := dMmu.io.phyAddr(31 downto 32 - config.cacheConfig.tagWidth)

      dCache.io.cacheOpEn := input(DCACHE_OP_EN)
      dCache.io.cacheOp := input(CACHE_OP)

      service[IfDescription].iCache.io.cacheOp.flush := pipelineSignal.isFlushed
      service[IfDescription].iCache.io.cacheOp.exception := output(EX_EN)
      service[IfDescription].iCache.io.cacheOp.en := pipelineSignal.isValid && !pipelineSignal.isStalled && input(ICACHE_OP_EN)
      service[IfDescription].iCache.io.cacheOp.cacheOp := input(CACHE_OP)
      service[IfDescription].iCache.io.cacheOp.addr := dMmu.io.phyAddr
      service[IfDescription].iCache.io.cacheOp.correctTag := dMmu.io.phyAddr(31 downto 32 - config.cacheConfig.tagWidth)

      val dRefill = dMmu.io.tlbException.refill
      val dInvalid = dMmu.io.tlbException.invalid
      val dModified = dMmu.io.tlbException.modified
      output(EX_EN) := input(EX_EN) | dRefill | dInvalid | dModified
      output(EX_OP) := PriorityMux(
        Vec(
          input(EX_EN),
          dRefill,
          dInvalid,
          dModified
        ),
        Vec(
          input(EX_OP),
          input(MEM_W) ? Exception.TLBS() | Exception.TLBL(),
          input(MEM_W) ? Exception.TLBS() | Exception.TLBL(),
          Exception.MOD()
        )
      )
      output(EX_TLB_HIT) := input(EX_EN) ? input(EX_TLB_HIT) | (output(EX_EN) && dInvalid)
      cp0.io.ex := pipelineSignal.isValid && !pipelineSignal.isStalled && output(EX_EN)
      cp0.io.exOp := output(EX_OP)
      cp0.io.tlbHit := output(EX_TLB_HIT)
      cp0.io.badPc := input(PC)
      cp0.io.delaySlot := input(DELAY_SLOT)
      output(EX_BAD_ADDR) := input(EX_EN) ? input(EX_BAD_ADDR) | input(MEM_ADDRESS)
      cp0.io.badAddr := output(EX_BAD_ADDR)
      output(EX_BAD_TLB_REQUEST).VPN := input(EX_EN) ? input(EX_BAD_TLB_REQUEST).VPN | dMmu.io.tlb.VPN
      output(EX_BAD_TLB_REQUEST).ASID := input(EX_EN) ? input(EX_BAD_TLB_REQUEST).ASID | cp0.io.tlbpDout.ASID
      cp0.io.badTLBRequest := output(EX_BAD_TLB_REQUEST)
      service[IfDescription].pcManager.io.isExceptionFlushed := (pipelineSignal.isValid && !pipelineSignal.isStalled && (output(EX_EN) || input(ERET)))
      service[IfDescription].pcManager.io.exceptionTrapAddr := cp0.io.trapPc
      service[IfDescription].pcManager.io.isTLBFlushed := pipelineSignal.isValid && !pipelineSignal.isStalled && input(TLB_OP_EN) && (input(TLB_OP) =/= TlbOp.TLBP)
      service[IfDescription].pcManager.io.tlbInstructionNextAddr := input(DELAY_SLOT) ? input(DELAY_SLOT_NEXT_PC) | (input(PC) + 4)
      EX.pipelineSignal.flush := pipelineSignal.isValid && !pipelineSignal.isStalled && (output(EX_EN) || input(ERET) || (input(TLB_OP_EN) && (input(TLB_OP) =/= TlbOp.TLBP)))

      cp0.io.eret := pipelineSignal.isValid && !pipelineSignal.isStalled && !output(EX_EN) && input(ERET)

      cp0.io.en := pipelineSignal.isValid && !pipelineSignal.isStalled && input(CP0_W)
      cp0.io.addr := input(CP0_REG)
      cp0.io.select := input(CP0_SELECT)
      cp0.io.din := input(CP0_DIN)
      insert(CP0_RESULT) := cp0.io.dout

      ByPassNetWorkService.io.sValid(1) := pipelineSignal.isValid && input(WB_EN)
      ByPassNetWorkService.io.sReady(1) := !input(MEM_EN)
      ByPassNetWorkService.io.sAddr(1) := input(WB_REG)
      ByPassNetWorkService.io.sDin(1) := input(WB_SRC).mux(
        WbSrc.Alu -> input(ALU_RESULT),
        WbSrc.Mdu -> input(MDU_RESULT),
        WbSrc.Ju -> input(JU_LINK),
        WbSrc.Cp0 -> input(CP0_RESULT),
        default -> U(0, 32 bits)
      )

      insert(DEBUG_COUNT) := cp0.io.debug.count
      insert(DEBUG_RANDOM) := cp0.io.debug.random
      insert(DEBUG_CAUSE) := cp0.io.debug.cause
    }

    MEM2 plug new Area {
      import MEM2._
      pipelineSignal.flush := False
      pipelineSignal.stall := dCache.io.isStalled

      dgu.io.addr := input(MEM_ADDRESS)
      dgu.io.memOp := input(MEM_OP)
      dgu.io.din1 := dCache.io.dout
      dgu.io.din2 := input(MEM_DIN)
      insert(MEM_RESULT) := dgu.io.dout

      service[IfDescription].iCache.io.cacheOp.isStalled := dCache.io.isStalled
      service[IfDescription].iCache.io.cacheOp.ready := dCache.io.ready

      ByPassNetWorkService.io.sValid(2) := pipelineSignal.isValid && input(WB_EN)
      ByPassNetWorkService.io.sAddr(2) := input(WB_REG)
      ByPassNetWorkService.io.sDin(2) := input(WB_SRC).mux(
        WbSrc.Alu -> input(ALU_RESULT),
        WbSrc.Mdu -> input(MDU_RESULT),
        WbSrc.Ju -> input(JU_LINK),
        WbSrc.Cp0 -> input(CP0_RESULT),
        WbSrc.DCache -> input(MEM_RESULT)
      )
    }
  }
}
