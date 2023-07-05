package j1cpu.cpu.plugins.ID

import j1cpu.cpu.{J1cpuConfig, RegFillConfig}
import spinal.core._
import spinal.lib._

class RegFill(regFillConfig: RegFillConfig) extends Component {
  val io = new Bundle {
    val rPorts = new Area {
      val addr = in Vec(UInt(5 bits), regFillConfig.rPorts)
      val dout = out Vec(UInt(32 bits), regFillConfig.rPorts)
    }

    val wPorts = new Area {
      val addr = in Vec(UInt(5 bits), regFillConfig.wPorts)
      val din = in Vec(UInt(32 bits), regFillConfig.wPorts)
    }
  }
  noIoPrefix()

  import io._

  val regFill = Mem(UInt (32 bits), 32)

  for (i <- 0 until regFillConfig.rPorts) {
    rPorts.dout(i) := regFill.readAsync(
      address = rPorts.addr(i)
    )
  }

  for (i <- 0 until regFillConfig.wPorts) {
    regFill.write(
      address = wPorts.addr(i),
      data = wPorts.din(i)
    )
  }
}

object regFillGen {
  def main(args: Array[String]): Unit = {
    val spinalConfig = SpinalConfig(
      targetDirectory = "hw/gen",
      defaultConfigForClockDomains = J1cpuConfig().clockConfig
    )

    spinalConfig.generateVerilog(new RegFill(RegFillConfig(2, 1)))
  }
}