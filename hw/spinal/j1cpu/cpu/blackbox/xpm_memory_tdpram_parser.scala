package j1cpu.cpu.blackbox

import spinal.core._

class xpm_memory_tdpram_parser(depth: Int, width: Int, use4Data: Int) extends Component {
  val io = new Bundle {
    // a for write
    val ena = in Bool()
    val wea = in Bits ((if (use4Data == 0) 1 else width / 8) bits)
    val addra = in UInt (log2Up(depth) bits)
    val dina = in UInt (width bits)
    val douta = out UInt (width bits)

    // b for read
    val enb = in Bool()
    val web = in Bits ((if (use4Data == 0) 1 else width / 8) bits)
    val addrb = in UInt (log2Up(depth) bits)
    val dinb = in UInt (width bits)
    val doutb = out UInt (width bits)
  }

  val mem = new xpm_memory_tdpram(depth: Int, width: Int, use4Data: Int)
  import io._

  val enaReg = Reg(Bool())
  val weaReg = Reg(Bits((if (use4Data == 0) 1 else (width / 8)) bits))
  val addraReg = Reg(UInt(log2Up(depth) bits))
  val dinaReg = Reg(UInt(width bits))

  val enbReg = Reg(Bool())
  val webReg = Reg(Bits((if (use4Data == 0) 1 else (width / 8)) bits))
  val addrbReg = Reg(UInt(log2Up(depth) bits))
  val dinbReg = Reg(UInt(width bits))

  enaReg := ena
  weaReg := wea
  addraReg := addra
  dinaReg := dina

  enbReg := enb
  webReg := web
  addrbReg := addrb
  dinbReg := dinb

  val readAnsa = UInt(width bits)
  mem.io.ena := ena
  mem.io.wea := wea
  mem.io.addra := addra
  mem.io.dina := dina
  readAnsa := mem.io.douta

  val readAnsb = UInt(width bits)
  mem.io.enb := enb
  mem.io.web := web
  mem.io.addrb := addrb
  mem.io.dinb := dinb
  readAnsb := mem.io.doutb

  // forbidden write in the same time
  when(enaReg) {
    when(enbReg && webReg.orR && addrbReg === addraReg) {
      if (use4Data == 1) {
        val tmp = UInt(width bits)
        for (i <- 0 until width / 8) {
          val curRange = ((i + 1) * 8 - 1) downto (i * 8)
          tmp(curRange) := webReg(i).mux(
            False -> readAnsa(curRange),
            True -> dinbReg(curRange)
          )
        }
        douta := tmp
      }
      else {
        douta := dinbReg
      }
    }.otherwise {
      if (use4Data == 1) {
        val tmp = UInt(width bits)
        for (i <- 0 until width / 8) {
          val curRange = ((i + 1) * 8 - 1) downto (i * 8)
          tmp(curRange) := weaReg(i).mux(
            False -> readAnsa(curRange),
            True -> dinaReg(curRange)
          )
        }
        douta := tmp
      }
      else {
        douta := weaReg.orR ? dinaReg | readAnsa
      }
    }
  }.otherwise {
    douta := U(0, width bits)
  }
  when(enbReg) {
    when(enaReg && weaReg.orR && addraReg === addrbReg) {
      if (use4Data == 1) {
        val tmp = UInt(width bits)
        for (i <- 0 until width / 8) {
          val curRange = ((i + 1) * 8 - 1) downto (i * 8)
          tmp(curRange) := weaReg(i).mux(
            False -> readAnsb(curRange),
            True -> dinaReg(curRange)
          )
        }
        doutb := tmp
      }
      else {
        doutb := dinaReg
      }
    }.otherwise {
      if (use4Data == 1) {
        val tmp = UInt(width bits)
        for (i <- 0 until width / 8) {
          val curRange = ((i + 1) * 8 - 1) downto (i * 8)
          tmp(curRange) := webReg(i).mux(
            False -> readAnsb(curRange),
            True -> dinbReg(curRange)
          )
        }
        doutb := tmp
      }
      else {
        doutb := webReg.orR ? dinbReg | readAnsb
      }
    }
  }.otherwise {
    doutb := U(0, width bits)
  }
}
