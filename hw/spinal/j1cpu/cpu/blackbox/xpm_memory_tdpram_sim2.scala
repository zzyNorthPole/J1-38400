package j1cpu.cpu.blackbox

import spinal.core._

class xpm_memory_tdpram_sim2(depth: Int, width: Int) extends Component {
  val io = new Bundle {
    val ena = in Bool()
    val wea = in Bits(1 bits)
    val addra = in UInt(log2Up(depth) bits)
    val dina = in UInt(width bits)
    val douta = out UInt(width bits)

    val enb = in Bool()
    val web = in Bits(1 bits)
    val addrb = in UInt(log2Up(depth) bits)
    val dinb = in UInt(width bits)
    val doutb = out UInt(width bits)
  }
  noIoPrefix()

  val mem = RegInit(U(0, depth bits))

  import io._
  val enaReg = Reg(Bool())
  val weaReg = Reg(Bits(1 bits))
  val addraReg = Reg(UInt(log2Up(depth) bits))
  val dinaReg = Reg(UInt(width bits))

  val enbReg = Reg(Bool())
  val webReg = Reg(Bits(1 bits))
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

  when(ena) {
    when(wea(0)) {
      mem(addra) := dina(0)
    }
  }

  when(enb) {
    when(web(0)) {
      mem(addrb) := dinb(0)
    }
  }

  when(enaReg) {
    douta := U(0, (width - 1) bits) @@ mem(addraReg)
  }.otherwise {
    douta := U(0, width bits)
  }

  when(enbReg) {
    doutb := U(0, (width - 1) bits) @@ mem(addrbReg)
  }.otherwise {
    doutb := U(0, width bits)
  }
}
