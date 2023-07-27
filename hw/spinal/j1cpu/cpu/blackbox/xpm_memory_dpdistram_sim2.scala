package j1cpu.cpu.blackbox

import spinal.core._

class xpm_memory_dpdistram_sim2(depth: Int, width: Int) extends Component {
  val io = new Bundle {
    // a for write and read
    val ena = in Bool()
    val wea = in Bits (1 bits)
    val addra = in UInt (log2Up(depth) bits)
    val dina = in UInt (width bits)
    val douta = out UInt (width bits)

    // b for read
    val enb = in Bool()
    val addrb = in UInt (log2Up(depth) bits)
    val doutb = out UInt (width bits)
  }

  noIoPrefix()

  val mem = RegInit(U(0, depth bits))

  import io._
  when(ena) {
    when(wea(0)) {
      mem(addra) := dina(0)
    }
  }
  douta := U(0, width - 1 bits) @@ mem(addra).asUInt
  doutb := U(0, width - 1 bits) @@ mem(addrb).asUInt
}
