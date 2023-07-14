package j1cpu.cpu.plugins.WB

import j1cpu.cpu.J1cpu
import j1cpu.cpu.plugins.ID.IdDescription
import j1cpu.cpu.signals._
import j1cpu.cpu.vexriscv.Plugin
import spinal.core._
import spinal.lib._

class WbDescription extends Plugin[J1cpu] {
  override def build(pipeline: J1cpu): Unit = {
    import pipeline._
    import pipeline.signal._

    WB plug new Area {
      import WB._
      pipelineSignal.flush := False
      pipelineSignal.stall := False
      service[IdDescription].regFile.io.wPorts.en(0) := input(WB_EN)
      service[IdDescription].regFile.io.wPorts.addr(0) := input(WB_REG)
      service[IdDescription].regFile.io.wPorts.din(0) := input(WB_SRC).mux(
        WbSrc.Alu -> input(ALU_RESULT),
        WbSrc.Ju -> input(JU_LINK),
        WbSrc.DCache -> input(MEM_RESULT),
        default -> U(0, 32 bits)
      )
    }
  }
}
