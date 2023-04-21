package j1cpu.gen

import spinal.core._
import j1cpu.cpu.{MyCpuTop}

object CpuGen {
  def main(args: Array[String]): Unit = {
    val spinalConfig = SpinalConfig(
      targetDirectory = "hw/gen",
      defaultConfigForClockDomains = ClockDomainConfig(resetActiveLevel = LOW)
    )

    spinalConfig.generateVerilog(new mycpu())
  }
}
