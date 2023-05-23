package j1cpu.cpu.plugins.ex

import j1cpu.cpu.signals
import spinal.core._
import j1cpu.cpu.signals._

class Alu extends Component {
  // For all other data types, you may have to add some brackets around it. Sorry, this is a Scala limitation.
  val io = new Bundle {
    val aluSrc1, aluSrc2 = in UInt(32 bits)
    val aluOp = in(AluOp())
    val aluResult = out UInt(32 bits)
    val aluOverflow = out Bool()
  }
  noIoPrefix()

  import io._
  import AluOp._
  val shamt = aluSrc1(4 downto 0)
  aluResult := aluOp.mux(
    ADD -> (aluSrc1 + aluSrc2),
    SUB -> (aluSrc1 - aluSrc2),
    SLT -> (U(0, 31 bits) @@ (aluSrc1.asSInt < aluSrc2.asSInt).asUInt),
    SLTU -> (U(0, 31 bits) @@ (aluSrc1 < aluSrc2).asUInt),
    AND -> (aluSrc1 & aluSrc2),
    NOR -> (~(aluSrc1 | aluSrc2)),
    OR -> (aluSrc1 | aluSrc2),
    XOR -> (aluSrc1 ^ aluSrc2),
    SLL -> (aluSrc2 |<< shamt),
    SRL -> (aluSrc2 |>> shamt),
    SRA -> (aluSrc2.asSInt >> shamt).asUInt,
    LUI -> (aluSrc2(15 downto 0) @@ U(0, 16 bits)),
    default -> U(0, 32 bits)
  )

  aluOverflow := aluOp.mux(
    ADD -> (aluSrc1(31) === aluSrc2(31) && aluSrc1(31) =/= aluResult(31)),
    SUB -> (aluSrc1(31) =/= aluSrc2(31) && aluSrc1(31) =/= aluResult(31)),
    default -> Bool(false)
  )

}
