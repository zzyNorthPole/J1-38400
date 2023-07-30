package j1cpu.cpu.blackbox

import j1cpu.cpu.J1cpuConfig
import spinal.core._

class xpm_memory_tdpram_sim(depth: Int, width: Int, use4Data: Int) extends Component {
  val io = new Bundle {
    val ena = in Bool()
    val wea = in Bits ((if (use4Data == 0) 1 else width / 8) bits)
    val addra = in UInt(log2Up(depth) bits)
    val dina = in UInt(width bits)
    val douta = out UInt(width bits)

    val enb = in Bool()
    val web = in Bits ((if (use4Data == 0) 1 else width / 8) bits)
    val addrb = in UInt(log2Up(depth) bits)
    val dinb = in UInt(width bits)
    val doutb = out UInt(width bits)
  }

  noIoPrefix()

  val mem = Mem(UInt(width bits), depth)
  import io._

  val enaReg = Reg(Bool())
  val weaReg = Reg(Bits ((if (use4Data == 0) 1 else (width / 8)) bits))
  val addraReg = Reg(UInt (log2Up(depth) bits))
  val dinaReg = Reg(UInt (width bits))

  val enbReg = Reg(Bool())
  val webReg = Reg(Bits ((if (use4Data == 0) 1 else (width / 8)) bits))
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
  readAnsa := mem.readWriteSync(
    address = addra,
    data = dina,
    enable = ena,
    write = wea.orR,
    mask = wea
  )

  val readAnsb = UInt(width bits)
  readAnsb := mem.readWriteSync(
    address = addrb,
    data = dinb,
    enable = enb,
    write = web.orR,
    mask = web
  )

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
      douta := readAnsa
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
      doutb := readAnsb
    }
  }.otherwise {
    doutb := U(0, width bits)
  }
}

object xpm_memory_tdpramGen {
  def main(args: Array[String]): Unit = {
    val spinalConfig = SpinalConfig(
      targetDirectory = "hw/gen",
      defaultConfigForClockDomains = J1cpuConfig().clockConfig
    )
    spinalConfig.generateVerilog(new xpm_memory_tdpram_sim(256, 32, 0))
  }
}