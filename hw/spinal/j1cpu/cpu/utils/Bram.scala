package j1cpu.cpu.utils

import j1cpu.cpu.J1cpuConfig
import spinal.core._
import spinal.lib._
import j1cpu.cpu.blackbox.{xpm_memory_sdpram, xpm_memory_sdpram_sim, xpm_memory_sdpram_sim2, xpm_memory_tdpram, xpm_memory_tdpram_parser, xpm_memory_tdpram_sim, xpm_memory_tdpram_sim2}

// use4Data = 0 tag ram
// use4Data = 1 data ram
class Bram(depth: Int, width: Int, use4Data: Int, sim: Int) extends Component {
  val io = new Bundle {
    // a for write
    val ena = in Bool()
    val wea = in Bits ((if (use4Data == 0) 1 else (width / 8)) bits)
    val addra = in UInt (log2Up(depth) bits)
    val dina = in UInt (width bits)
    val douta = out UInt (width bits)

    // b for read
    val enb = in Bool()
    val web = in Bits ((if (use4Data == 0) 1 else (width / 8)) bits)
    val addrb = in UInt (log2Up(depth) bits)
    val dinb = in UInt (width bits)
    val doutb = out UInt (width bits)
  }

  noIoPrefix()

  if (sim == 2) {
    val bram = new xpm_memory_tdpram_sim2(depth, width)

    import io._

    bram.io.ena := ena
    bram.io.wea := wea
    bram.io.addra := addra
    bram.io.dina := dina
    douta := bram.io.douta

    bram.io.enb := enb
    bram.io.web := web
    bram.io.addrb := addrb
    bram.io.dinb := dinb
    doutb := bram.io.doutb
  }
  else if (sim == 0) {
    val bram = new xpm_memory_tdpram_parser(depth, width, use4Data)

    import io._

    bram.io.ena := ena
    bram.io.wea := wea
    bram.io.addra := addra
    bram.io.dina := dina
    douta := bram.io.douta

    bram.io.enb := enb
    bram.io.web := web
    bram.io.addrb := addrb
    bram.io.dinb := dinb
    doutb := bram.io.doutb
  }
  else {
    val bram = new xpm_memory_tdpram_sim(depth, width, use4Data)

    import io._

    bram.io.ena := ena
    bram.io.wea := wea
    bram.io.addra := addra
    bram.io.dina := dina
    douta := bram.io.douta

    bram.io.enb := enb
    bram.io.web := web
    bram.io.addrb := addrb
    bram.io.dinb := dinb
    doutb := bram.io.doutb
  }
}

object bramGen {
  def main(args: Array[String]): Unit = {
    val spinalConfig = SpinalConfig(
      targetDirectory = "hw/gen",
      defaultConfigForClockDomains = J1cpuConfig().clockConfig
    )

    spinalConfig.generateVerilog(new Bram(256, 1, 0, 2))
  }
}
