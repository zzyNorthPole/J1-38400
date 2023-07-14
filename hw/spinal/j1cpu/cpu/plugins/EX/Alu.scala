package j1cpu.cpu.plugins.EX

import j1cpu.cpu.signals
import spinal.core._
import j1cpu.cpu.signals._

class Alu extends Component {
  // For all other data types, you may have to add some brackets around it. Sorry, this is a Scala limitation.
  val io = new Bundle {
    val din1, din2 = in UInt (32 bits)
    val aluOp = in(AluOp())
    val dout = out UInt (32 bits)
    val aluOverflow = out Bool()
  }
  noIoPrefix()

  import io._

  import AluOp._
  val shamt = din1(4 downto 0)
  dout := aluOp.mux(
    ADD -> (din1.asSInt + din2.asSInt).asUInt,
    ADDU -> (din1 + din2),
    SUB -> (din1.asSInt - din2.asSInt).asUInt,
    SUBU -> (din1 - din2),
    AND -> (din1 & din2),
    OR -> (din1 | din2),
    XOR -> (din1 ^ din2),
    NOR -> (~(din1 | din2)),
    SLT -> (S(0, 31 bits) @@ (din1.asSInt < din2.asSInt)).asUInt,
    SLTU -> (U(0, 31 bits) @@ (din1 < din2)),
    SLL -> (din2 |<< shamt),
    SRL -> (din2 |>> shamt),
    SRA -> (din2 >> shamt),
    LUI -> (din2(15 downto 0) @@ U(0, 16 bits))
  )

  aluOverflow := aluOp.mux(
    ADD -> (din1(31) === din2(31) && din1(31) =/= dout(31)),
    SUB -> (din1(31) =/= din2(31) && din1(31) =/= dout(31)),
    default -> Bool(false)
  )

}
