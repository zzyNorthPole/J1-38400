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
      val we = in Vec(Bits(4 bits), regFillConfig.wPorts)
      val addr = in Vec(UInt(5 bits), regFillConfig.wPorts)
      val din = in Vec(UInt(32 bits), regFillConfig.wPorts)
    }
  }
  noIoPrefix()

  import io._

  val regFile = Vec(RegInit(U(0, 32 bits)), 32)

  for (i <- 0 until regFillConfig.rPorts) {
    rPorts.dout(i) := regFile(rPorts.addr(i))
  }

  for (i <- 0 until regFillConfig.wPorts) {
    when(wPorts.en(i) && (wPorts.addr(i) =/= U(0, 5 bits))) {
      for (j <- 0 to 3) {
        when(wPorts.we(i)(j) === True) {
          regFile(wPorts.addr(i))((j * 8 + 7) downto (j * 8)) := wPorts.din(i)((j * 8 + 7) downto (j * 8))
        }
      }
    }
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