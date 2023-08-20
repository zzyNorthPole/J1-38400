package j1cpu.cpu.plugins.EX

import j1cpu.cpu.signals.JuOp
import spinal.core._

class Ju extends Component {
  val io = new Bundle {
    val en = in Bool()
    val juOp = in(JuOp())
    val din1, din2 = in SInt(32 bits)
    val pc = in UInt(32 bits)
    val offset = in UInt(32 bits)
    val predictPc = in UInt(32 bits)
    val jumpPc = out UInt(32 bits)
    val link = out UInt(32 bits)
    val correctPc = out UInt(32 bits)
    val flush = out Bool()

    val bhr = new Bundle {
//      val en = in Bool()
      val din = in UInt(2 bits)
      val dout = out UInt(2 bits)
    }
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

  jumpPc := juOp.mux(
    JR -> din1.asUInt,
    JALR -> din1.asUInt,
    J -> offset,
    JAL -> offset,
    default -> (pc + 4 + offset)
  )

  link := pc + 8
  correctPc := result ? jumpPc | link

  flush := en && (correctPc =/= predictPc)

  bhr.dout := en ? (correctPc === predictPc).mux(
    False -> (
      bhr.din.mux(
        U(0, 2 bits) -> U(1, 2 bits),
        U(1, 2 bits) -> U(2, 2 bits),
        U(2, 2 bits) -> U(1, 2 bits),
        U(3, 2 bits) -> U(2, 2 bits)
      )
    ),
    True -> (
      bhr.din.mux(
        U(0, 2 bits) -> U(0, 2 bits),
        U(1, 2 bits) -> U(0, 2 bits),
        U(2, 2 bits) -> U(3, 2 bits),
        U(3, 2 bits) -> U(3, 2 bits)
      )
    )
  ) | (correctPc === predictPc).mux(
    False -> U(1, 2 bits),
    True -> U(0, 2 bits)
  )
}
