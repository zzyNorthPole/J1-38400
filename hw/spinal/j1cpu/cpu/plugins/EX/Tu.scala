package j1cpu.cpu.plugins.EX

import j1cpu.cpu.signals.TuOp
import spinal.core._

class Tu extends Component {
  val io = new Bundle {
    val en = in Bool()
    val tuOp = in(TuOp())
    val din1, din2 = in UInt(32 bits)
    val trap = out Bool()
  }
  noIoPrefix()
  import io._
  import TuOp._
  trap := en && tuOp.mux(
    TEQ -> (din1 === din2),
    TNE -> (din1 =/= din2),
    TGE -> (din1.asSInt >= din2.asSInt),
    TGEU -> (din1 >= din2),
    TLT -> (din1.asSInt < din2.asSInt),
    TLTU -> (din1 < din2)
  )
}
