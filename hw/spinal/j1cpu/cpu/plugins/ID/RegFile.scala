package j1cpu.cpu.plugins.ID

import j1cpu.cpu.{J1cpuConfig, RegFillConfig}
import spinal.core._
import spinal.lib._

class RegFile(regFillConfig: RegFillConfig) extends Component {
  val io = new Bundle {
    val rPorts = new Bundle {
      val addr = in Vec(UInt(5 bits), regFillConfig.rPorts)
      val dout = out Vec(UInt(32 bits), regFillConfig.rPorts)
    }

    val wPorts = new Bundle {
      val en = in Vec(Bool(), regFillConfig.wPorts)
      val addr = in Vec(UInt(5 bits), regFillConfig.wPorts)
      val din = in Vec(UInt(32 bits), regFillConfig.wPorts)
    }
  }
  noIoPrefix()

  import io._

  val regFile = Mem(UInt (32 bits), 32)

  for (i <- 0 until regFillConfig.rPorts) {
    rPorts.dout(i) := regFile.readAsync(
      address = rPorts.addr(i)
    )
  }

  for (i <- 0 until regFillConfig.wPorts) {
    regFile.write(
      address = wPorts.addr(i),
      data = wPorts.din(i),
      enable = wPorts.en(i) && (wPorts.addr(i) =/= U(0, 5 bits))
    )
  }
}

object regFileGen {
  def main(args: Array[String]): Unit = {
    val spinalConfig = SpinalConfig(
      targetDirectory = "hw/gen",
      defaultConfigForClockDomains = J1cpuConfig().clockConfig
    )

    spinalConfig.generateVerilog(new RegFile(RegFillConfig(2, 1)))
  }
}