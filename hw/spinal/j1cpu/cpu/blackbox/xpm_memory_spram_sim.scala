package j1cpu.cpu.blackbox

import spinal.core._
import spinal.lib._

class xpm_memory_spram_sim(depth: Int, width: Int, use4Data: Int) extends Component {

  val io = new Bundle {
    val clka = in Bool()
    val ena = in Bool()
    val wea = in UInt((if (use4Data == 0) 1 else (width / 8)) bits)
    val addra = in UInt(log2Up(depth) bits)
    val dina = in UInt(width bits)
    val douta = out UInt(width bits)
  }

  noIoPrefix()

  // default read first ram
  // read first: dout = previous mem[addr]
  // write first: if read dout = previous mem[addr] else dout = previous din
  // no change: if read dout = previous mem[addr] else dout not change
  val mem = Mem(UInt(width bits), depth)
  import io._

  new ClockingArea(
    new ClockDomain(
      clock = clka,
      config = ClockDomainConfig(
        resetActiveLevel = HIGH,
        resetKind = SYNC,
        clockEdge = RISING
      )
    )
  ) {
    io.douta := mem.readWriteSyncMixedWidth(
      address = addra,
      data = dina,
      enable = ena,
      write = wea.orR,
      mask = wea.asBits
    )
  }
}

//object memGen {
//  def main(args: Array[String]): Unit = {
//    val spinalConfig = SpinalConfig(
//      targetDirectory = "hw/gen",
//      defaultConfigForClockDomains = ClockDomainConfig(
//        resetActiveLevel = HIGH,
//        resetKind = SYNC,
//        clockEdge = RISING
//      )
//    )
//
//    spinalConfig.generateVerilog(new xpm_memory_spram_sim(256, 21, 0))
//  }
//}
