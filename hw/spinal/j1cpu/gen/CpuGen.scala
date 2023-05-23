package j1cpu.gen

import spinal.core._
import j1cpu.cpu._

object CpuGen {
  def main(args: Array[String]): Unit = {
    val spinalConfig = SpinalConfig(
      targetDirectory = "hw/gen",
      defaultConfigForClockDomains = ClockDomainConfig(
        resetActiveLevel = HIGH,
        resetKind = SYNC,
        clockEdge = RISING
      )
    )

    spinalConfig.generateVerilog(new J1cpu(new J1cpuSignal))
  }
}
