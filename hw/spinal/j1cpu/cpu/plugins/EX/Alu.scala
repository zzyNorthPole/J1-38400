package j1cpu.cpu.plugins.EX

import j1cpu.cpu.{J1cpuConfig, signals}
import spinal.core._
import j1cpu.cpu.signals._

class Alu extends Component {
  // For all other data types, you may have to add some brackets around it. Sorry, this is a Scala limitation.
  val io = new Bundle {
    val en = in Bool()
    val aluOp = in(AluOp())
    val din1, din2 = in UInt (32 bits)
    val dout = out UInt (32 bits)
    val movFail = out Bool()
    val overflow = out Bool()
  }
  noIoPrefix()

  def cl(flag: Bool, din: UInt): UInt = {
    val rev = UInt(32 bits)
    rev := flag ? ~din.reversed | din.reversed
    val tmp = Vec(Vec(UInt(16 bits), 2), 5)
    val check = Vec(Bool(), 5)
    for (i <- 4 downto 0) {
      for (j <- 0 until 2) {
        if (i == 4) {
          tmp(i)(j) := rev(((j + 1) * (1 << i) - 1) downto (j * (1 << i)))
        }
        else {
          tmp(i)(j) := U(0, (16 - (1 << i)) bits) @@ tmp(i + 1)(check(i + 1).asUInt)(((j + 1) * (1 << i) - 1) downto (j * (1 << i)))
        }
      }
    }
    for (i <- 4 downto 0) {
      check(i) := (tmp(i)(0) === U(0, 16 bits))
    }
    val curDout = UInt(5 bits)
    for (i <- 4 downto 0) curDout(i) := check(i)
    val dout = UInt(32 bits)
    dout := (curDout.resize(6 bits) + ((tmp(0)(0) === U(0, 16 bits)) & (tmp(0)(1) === U(0, 16 bits))).asUInt.resize(6 bits)).resize(32 bits)
    dout
  }

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
    SRA -> (din2.asSInt >> shamt).asUInt,
    LUI -> (din2(15 downto 0) @@ U(0, 16 bits)),
    MOVN -> din1,
    MOVZ -> din1,
    CLO -> cl(True, din1),
    CLZ -> cl(False, din1)
  )

  movFail := en && aluOp.mux(
    MOVN -> (din2 === U(0, 32 bits)),
    MOVZ -> (din2 =/= U(0, 32 bits)),
    default -> False
  )

  overflow := en && aluOp.mux(
    ADD -> (din1(31) === din2(31) && din1(31) =/= dout(31)),
    SUB -> (din1(31) =/= din2(31) && din1(31) =/= dout(31)),
    default -> False
  )

}

object AluGen {
  def main(args: Array[String]): Unit = {
    val spinalConfig = SpinalConfig(
      targetDirectory = "hw/gen",
      defaultConfigForClockDomains = J1cpuConfig().clockConfig
    )

    spinalConfig.generateVerilog(new Alu())
  }
}