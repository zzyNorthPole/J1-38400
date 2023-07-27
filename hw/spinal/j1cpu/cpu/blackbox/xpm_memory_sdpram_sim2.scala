package j1cpu.cpu.blackbox

import spinal.core._

class xpm_memory_sdpram_sim2(depth: Int, width: Int) extends Component {
  val io = new Bundle {
    // a for write
    val ena = in Bool()
    val wea = in Bits(1 bits)
    val addra = in UInt(log2Up(depth) bits)
    val dina = in UInt(width bits)

    // b for read
    val enb = in Bool()
    val addrb = in UInt(log2Up(depth) bits)
    val doutb = out UInt(width bits)
  }

  noIoPrefix()

  val mem = RegInit(U(0, depth bits))

  import io._
  val enaReg = Reg(Bool())
  val weaReg = Reg(Bits(1 bits))
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

  when(ena) {
    when(wea(0)) {
      mem(addra) := dina(0)
    }
  }

  when(enbReg) {
    doutb := U(0, (width - 1) bits) @@ mem(addrbReg)
  }.otherwise {
    doutb := U(0, width bits)
  }
}
