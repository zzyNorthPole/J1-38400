package j1cpu.cpu.blackbox

import spinal.core._

class xpm_memory_sdpram_sim(depth: Int, width: Int, use4Data: Int) extends Component {
  val io = new Bundle {
    val clka = in Bool()

    // a for write
    val ena = in Bool()
    val wea = in UInt((if (use4Data == 0) 1 else (width / 8)) bits)
    val addra = in UInt(log2Up(depth) bits)
    val dina = in UInt(width bits)

    // b for read
    val enb = in Bool()
    val addrb = in UInt(log2Up(depth) bits)
    val doutb = out UInt(width bits)
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
    val enaReg = Reg(Bool())
    val weaReg = Reg(UInt((if (use4Data == 0) 1 else (width / 8)) bits))
    val addraReg = Reg(UInt(log2Up(depth) bits))
    val dinaReg = Reg(UInt(width bits))

    // b for read
    val enbReg = Reg(Bool())
    val addrbReg = Reg(UInt(log2Up(depth) bits))

    enaReg := ena
    weaReg := wea
    addraReg := addra
    dinaReg := dina

    enbReg := enb
    addrbReg := addrb

    mem.write(
      address = addra,
      data = dina,
      enable = ena,
      mask = wea.asBits
    )

    val readAns = UInt(width bits)
    readAns := mem.readSync(
      address = addrb,
      enable = enb
    )


    when(enbReg) {
      when(enaReg && addraReg === addrbReg) {
        if (use4Data == 1) {
          val tmp = UInt(width bits)
          for (i <- 0 until (width / 8)) {
            val curRange = ((i + 1) * 8 - 1) downto (i * 8)
            tmp(curRange) := weaReg(i).mux(
              False -> readAns(curRange),
              True -> dinaReg(curRange)
            )
          }
          doutb := tmp
        }
        else {
          doutb := dinaReg
        }
      }.otherwise {
        doutb := readAns
      }
    }.otherwise {
      doutb := U(0, width bits)
    }
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
//    spinalConfig.generateVerilog(new xpm_memory_sdpram_sim(256, 32, 1))
//  }
//}