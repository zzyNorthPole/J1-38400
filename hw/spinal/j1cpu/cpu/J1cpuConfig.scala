package j1cpu.cpu

import spinal.core._
import spinal.lib.bus.amba4.axi.Axi4Config

case class RegFillConfig(
  rPorts: Int,
  wPorts: Int
                        ) {

}

case class ByPassConfig(
  decoderPass: Int,
  backendStage: Int,
  backendUnfinishedStage:Int,
  backendPass: Int
                       ) {

}
case class CacheConfig(
  ways: Int,
  lines: Int,
  blockSize: Int
                      ) {
  val indexWidth = log2Up(lines)
  val offsetWidth = log2Up(blockSize)
  val tagWidth = 32 - indexWidth - offsetWidth
  val words = blockSize / 4
}

case class TlbConfig(
  use: Boolean,
  lines: Int
                    ) {

}

case class J1cpuConfig() {
  val sim = 1

  val clockConfig = ClockDomainConfig(
    resetActiveLevel = HIGH,
    resetKind = SYNC,
    clockEdge = RISING
  )

  val regFillConfig = RegFillConfig(
    rPorts = 2,
    wPorts = 1
  )

  val byPassConfig = ByPassConfig(
    decoderPass = 2,
    backendStage = 4,
    backendUnfinishedStage = 2,
    backendPass = 1
  )

  val axiConfig = Axi4Config(
    addressWidth = 32,
    dataWidth = 32,
    idWidth = 4,
    useRegion = false,
    useQos = false,
    useLock = false
  )

  val cacheConfig = CacheConfig(
    ways = 2,
    lines = 1 << 8,
    blockSize = 16
  )
  val tlbConfig = TlbConfig(
    use = true,
    lines = 1 << 3
  )
}
