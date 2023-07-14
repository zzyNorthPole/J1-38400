package j1cpu.cpu.plugins.EX

import j1cpu.cpu.signals.JuOp
import spinal.core._

class Ju extends Component {
  val io = new Bundle {
    val juOp = in(JuOp())
    val din1 = in SInt(32 bits)
    val din2 = in SInt(32 bits)
    val pc = in UInt(32 bits)
    val offset = in UInt(32 bits)
    val predictPc = in UInt(32 bits)
    val link = out UInt(32 bits)
    val correctPc = out UInt(32 bits)
    val flush = out Bool()
  }
  noIoPrefix()
  import io._

  val result = Bool()
  import JuOp._
  result := juOp.mux(
    BLTZ -> (din1 < 0),
    BGEZ -> (din1 >= 0),
    BLTZAL -> (din1 < 0),
    BGEZAL -> (din1 >= 0),
    BEQ -> (din1 === din2),
    BNE -> (din1 =/= din2),
    BLEZ -> (din1 <= 0),
    BGTZ -> (din1 > 0),
    default -> True
  )

  val jumpPc = UInt(32 bits)
  jumpPc := juOp.mux(
    JR -> din1.asUInt,
    JALR -> din1.asUInt,
    J -> offset,
    JAL -> offset,
    default -> (pc + offset)
  )

  link := pc + 8
  correctPc := result ? jumpPc | link

  flush := correctPc === predictPc
}
