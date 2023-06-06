package j1cpu.cpu

import spinal.core._
import spinal.lib.bus.amba4.axi.Axi4Config

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

class J1cpuConfig {
  val clockConfig = ClockDomainConfig(
    resetActiveLevel = HIGH,
    resetKind = SYNC,
    clockEdge = RISING
  )

  val axiConfig = Axi4Config(
    addressWidth = 32,
    dataWidth = 32,
    idWidth = 5,
    useRegion = false,
    useQos = false
  )
  val cacheConfig = CacheConfig(
    ways = 2,
    lines = 1 << 20,
    blockSize = 32
  )
  val tlbConfig = TlbConfig(
    use = true,
    lines = 1 << 3
  )
}
