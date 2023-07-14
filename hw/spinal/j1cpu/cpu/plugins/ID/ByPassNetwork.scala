package j1cpu.cpu.plugins.ID

import j1cpu.cpu.{ByPassConfig, J1cpuConfig}
import spinal.core._
import spinal.lib._

class ByPassNetwork(byPassConfig: ByPassConfig) extends Component {
  val io = new Bundle {
    val sValid = in Vec(Bool(), byPassConfig.backendPass * byPassConfig.backendStage)
    val sReady = in Vec(Bool(), byPassConfig.backendUnfinishedStage * byPassConfig.backendPass)
    val sAddr = in Vec(UInt(5 bits), byPassConfig.backendStage * byPassConfig.backendPass)
    val sDin = in Vec(UInt(32 bits), byPassConfig.backendStage * byPassConfig.backendPass)
    val tValid = in Vec(Bool(), byPassConfig.decoderPass)
    val tAddr = in Vec(UInt(5 bits), byPassConfig.decoderPass)
    val tDin = in Vec(UInt(32 bits), byPassConfig.decoderPass)
    val tDout = out Vec(UInt(32 bits), byPassConfig.decoderPass)
    val stall = out Vec(Bool(), byPassConfig.decoderPass)
  }
  noIoPrefix()
  import io._

  val hits = Vec(Vec(Bool(), byPassConfig.backendPass * byPassConfig.backendStage), byPassConfig.decoderPass)
  for (i <- 0 until byPassConfig.decoderPass) {
    for (j <- 0 until byPassConfig.backendStage) {
      for (k <- 0 until byPassConfig.backendPass) {
        val tIdx = j * byPassConfig.backendPass + byPassConfig.backendPass - 1 - k
        val sIdx = j * byPassConfig.backendPass + k
        hits(i)(tIdx) := sValid(sIdx) && (sAddr(sIdx) === tAddr(i))
      }
    }
  }

  val hitInvalids = Vec(Vec(Bool(), byPassConfig.backendPass * byPassConfig.backendUnfinishedStage), byPassConfig.decoderPass)
  val hitMaybeInvalids = Vec(Vec(Bool(), byPassConfig.backendPass * byPassConfig.backendUnfinishedStage), byPassConfig.decoderPass)
  for (i <- 0 until byPassConfig.decoderPass) {
    for (j <- 0 until byPassConfig.backendUnfinishedStage) {
      for (k <- 0 until byPassConfig.backendPass) {
        val tIdx = j * byPassConfig.backendPass + byPassConfig.backendPass - 1 - k
        val sIdx = j * byPassConfig.backendPass + k
        hitInvalids(i)(tIdx) := hits(i)(tIdx) && sReady(sIdx)
        hitMaybeInvalids(i)(tIdx) := hits(i)(tIdx)
      }
    }
  }
  for (i <- 0 until byPassConfig.decoderPass) {
    tDout(i) := hits(i).orR ? PriorityMux(hits(i), sDin) | tDin(i)
  }

  for (i <- 0 until byPassConfig.decoderPass) {
    stall(i) := tValid(i) &&
      (OHMasking.first(hitMaybeInvalids(i)) === OHMasking.first(hitInvalids(i)))
  }
}

object byPassNetWorkGen {
  def main(args: Array[String]): Unit = {
    val spinalConfig = SpinalConfig(
      targetDirectory = "hw/gen",
      defaultConfigForClockDomains = J1cpuConfig().clockConfig
    )

    spinalConfig.generateVerilog(new ByPassNetwork(ByPassConfig(2, 4, 2, 2)))
  }
}