package j1cpu.cpu.blackbox

import spinal.core._
import spinal.lib._
import j1cpu.cpu.J1cpuConfig

class xpm_memory_dpdistram_sim(depth: Int, width: Int, use4Data: Int) extends Component {
  val io = new Bundle {
    // a for write and read
    val ena = in Bool()
    val wea = in Bits ((if (use4Data == 0) 1 else (width / 8)) bits)
    val addra = in UInt (log2Up(depth) bits)
    val dina = in UInt (width bits)
    val douta = out UInt (width bits)

    // b for read
    val enb = in Bool()
    val addrb = in UInt (log2Up(depth) bits)
    val doutb = out UInt (width bits)
  }

  noIoPrefix()

  val mem = Mem(UInt (width bits), depth)

  import io._

  mem.write(
    address = addra,
    data = dina,
    enable = ena,
    mask = wea
  )
  douta := mem.readAsync(
    address = addra
  )
  doutb := mem.readAsync(
    address = addrb
  )
}

object xpm_memory_dpdistramGen {
  def main(args: Array[String]): Unit = {
    val spinalConfig = SpinalConfig(
      targetDirectory = "hw/gen",
      defaultConfigForClockDomains = J1cpuConfig().clockConfig
    )

    spinalConfig.generateVerilog(new xpm_memory_dpdistram_sim(256, 21, 0))
  }
}