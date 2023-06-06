package j1cpu.cpu.blackbox

import spinal.core._
import spinal.lib._

class xpm_memory_dpdistram_sim(depth: Int, width: Int, use4Data: Int) extends Component {
  val io = new Bundle {
    val clka = in Bool()

    // a for write and read
    val ena = in Bool()
    val wea = in UInt ((if (use4Data == 0) 1 else (width / 8)) bits)
    val addra = in UInt (log2Up(depth) bits)
    val dina = in UInt (width bits)
    val douta = out UInt (width bits)

    // b for read
    val enb = in Bool()
    val addrb = in UInt (log2Up(depth) bits)
    val doutb = out UInt (width bits)
  }

  noIoPrefix()

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
    mem.write(
      address = addra,
      data = dina,
      enable = ena,
      mask = wea.asBits
    )
    douta := mem.readAsync(
      address = addra
    )
    doutb := mem.readAsync(
      address = addrb
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
//    spinalConfig.generateVerilog(new xpm_memory_dpdistram_sim(256, 21, 0))
//  }
//}