package j1cpu.cpu.utils

import j1cpu.cpu.J1cpuConfig
import spinal.core._
import spinal.lib._
import j1cpu.cpu.blackbox.{xpm_memory_sdpram, xpm_memory_sdpram_sim}

// use4Data = 0 tag ram
// use4Data = 1 data ram
class Bram(depth: Int, width: Int, use4Data: Int, sim: Int) extends Component {
  val io = new Bundle {
    // a for write
    val ena = in Bool()
    val wea = in Bits ((if (use4Data == 0) 1 else (width / 8)) bits)
    val addra = in UInt (log2Up(depth) bits)
    val dina = in UInt (width bits)

    // b for read
    val enb = in Bool()
    val addrb = in UInt (log2Up(depth) bits)
    val doutb = out UInt (width bits)
  }

  noIoPrefix()

  if (sim == 0) {
    val bram = new xpm_memory_sdpram(depth, width, use4Data)

    import io._

    bram.io.ena := ena
    bram.io.wea := wea
    bram.io.addra := addra
    bram.io.dina := dina

    bram.io.enb := enb
    bram.io.addrb := addrb
    doutb := bram.io.doutb
  }
  else {
    val bram = new xpm_memory_sdpram_sim(depth, width, use4Data)

    import io._

    bram.io.ena := ena
    bram.io.wea := wea
    bram.io.addra := addra
    bram.io.dina := dina

    bram.io.enb := enb
    bram.io.addrb := addrb
    doutb := bram.io.doutb
  }
}

object bramGen {
  def main(args: Array[String]): Unit = {
    val spinalConfig = SpinalConfig(
      targetDirectory = "hw/gen",
      defaultConfigForClockDomains = J1cpuConfig().clockConfig
    )

    spinalConfig.generateVerilog(new Bram(256, 32, 1, 0))
  }
}
