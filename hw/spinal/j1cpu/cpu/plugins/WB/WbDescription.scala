package j1cpu.cpu.plugins.WB

import j1cpu.cpu.J1cpu
import j1cpu.cpu.plugins.ID.IdDescription
import j1cpu.cpu.signals._
import j1cpu.cpu.vexriscv.Plugin
import spinal.core._
import spinal.lib._

class WbDescription extends Plugin[J1cpu] {
  val debug = new Bundle {
    val pc = out UInt (32 bits)
    val rfWen = out Bits (4 bits)
    val rfWnum = out UInt (5 bits)
    val rfWdata = out UInt (32 bits)
    val cp0Count = out UInt (32 bits)
    val cp0Random = out UInt (32 bits)
    val cp0Cause = out UInt (32 bits)
    val int = out Bool()
    val commit = out Bool()
  }
  override def build(pipeline: J1cpu): Unit = {
    import pipeline._
    import pipeline.signal._

    WB plug new Area {
      import WB._
      pipelineSignal.flush := False
      pipelineSignal.stall := False
      val writeBackDin = input(WB_SRC).mux(
        WbSrc.Alu -> input(ALU_RESULT),
        WbSrc.Ju -> input(JU_LINK),
        WbSrc.Mdu -> input(MDU_RESULT),
        WbSrc.Cp0 -> input(CP0_RESULT),
        WbSrc.DCache -> input(MEM_RESULT)
      )
      service[IdDescription].regFile.io.wPorts.en(0) := pipelineSignal.isValid && input(WB_EN) && !input(EX_EN)
      service[IdDescription].regFile.io.wPorts.we(0) := B"4'b1111"
      service[IdDescription].regFile.io.wPorts.addr(0) := input(WB_REG)
      service[IdDescription].regFile.io.wPorts.din(0) := writeBackDin

      service[IdDescription].byPassNetwork.io.sValid(3) := pipelineSignal.isValid && input(WB_EN)
      service[IdDescription].byPassNetwork.io.sAddr(3) := input(WB_REG)
      service[IdDescription].byPassNetwork.io.sDin(3) := writeBackDin

      debug.pc := input(PC)
      debug.rfWen := B(4 bits, default -> (pipelineSignal.isValid && input(WB_EN) && !input(EX_EN)))
      debug.rfWnum := input(WB_REG)
      debug.rfWdata := writeBackDin
      debug.cp0Count := input(DEBUG_COUNT)
      debug.cp0Random := input(DEBUG_RANDOM)
      debug.cp0Cause := input(DEBUG_CAUSE)
      debug.int := pipelineSignal.isValid && input(EX_EN) && (input(EX_OP) === Exception.INT)
      debug.commit := pipelineSignal.isValid
    }
  }
}
