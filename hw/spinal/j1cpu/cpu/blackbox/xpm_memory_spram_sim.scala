package j1cpu.cpu.blackbox

import j1cpu.cpu.J1cpuConfig
import spinal.core._
import spinal.lib._

class xpm_memory_spram_sim(depth: Int, width: Int, use4Data: Int) extends Component {

  val io = new Bundle {
    val ena = in Bool()
    val wea = in Bits ((if (use4Data == 0) 1 else (width / 8)) bits)
    val addra = in UInt (log2Up(depth) bits)
    val dina = in UInt (width bits)
    val douta = out UInt (width bits)
  }

  noIoPrefix()

  // default read first ram
  // read first: dout = previous mem[addr]
  // write first: if read dout = previous mem[addr] else dout = previous din
  // no change: if read dout = previous mem[addr] else dout not change
  val mem = Mem(UInt (width bits), depth)
  import io._

  io.douta := mem.readWriteSyncMixedWidth(
    address = addra,
    data = dina,
    enable = ena,
    write = wea.orR,
    mask = wea
  )
}

object xpm_memory_spramGen {
  def main(args: Array[String]): Unit = {
    val spinalConfig = SpinalConfig(
      targetDirectory = "hw/gen",
      defaultConfigForClockDomains = J1cpuConfig().clockConfig
    )

    spinalConfig.generateVerilog(new xpm_memory_spram_sim(256, 21, 0))
  }
}