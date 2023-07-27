package j1cpu.cpu.plugins.EX

import j1cpu.cpu.J1cpuConfig
import j1cpu.cpu.signals.MduOp
import j1cpu.cpu.utils.Div
import spinal.core._

class Mdu extends Component {
  val io = new Bundle {
    val flush = in Bool()
    val en = in Bool()
    val mduOp = in(MduOp())
    val din1, din2 = in UInt (32 bits)
    val dout = out UInt (32 bits)
    val ready = out Bool()
  }
  noIoPrefix()

  import io._
  import MduOp._

  val unsignedMul = UInt(64 bits)
  val signedMul = UInt(64 bits)
  unsignedMul := (din1 * din2)
  signedMul := (din1.asSInt * din2.asSInt).asUInt
  val div = new Div(32)
  div.io.en := en && (mduOp === MduOp.DIV || mduOp === MduOp.DIVU)
  div.io.signed := mduOp === MduOp.DIV
  div.io.din1 := din1
  div.io.din2 := din2
  ready := !en || (en && ((mduOp === MduOp.DIV || mduOp === MduOp.DIVU) ? div.io.ready | True))

  val Hi = RegInit(U(0, 32 bits))
  val Lo = RegInit(U(0, 32 bits))
  when(en && io.ready && !flush && (mduOp === MULT || mduOp === MULTU || mduOp === MTHI || mduOp === DIV || mduOp === DIVU)) {
    Hi := mduOp.mux(
      MULT -> signedMul(63 downto 32),
      MULTU -> unsignedMul(63 downto 32),
      MTHI -> din1,
      DIV -> div.io.dout2,
      DIVU -> div.io.dout2,
      default -> U(0, 32 bits)
    )
  }
  when(en && io.ready && !flush && (mduOp === MULT || mduOp === MULTU || mduOp === MTLO || mduOp === DIV || mduOp === DIVU)) {
    Lo := mduOp.mux(
      MULT -> signedMul(31 downto 0),
      MULTU -> unsignedMul(31 downto 0),
      MTLO -> din1,
      DIV -> div.io.dout1,
      DIVU -> div.io.dout1,
      default -> U(0, 32 bits)
    )
  }
  dout := mduOp.mux(
    MUL -> signedMul(31 downto 0),
    MFHI -> Hi,
    MFLO -> Lo,
    default -> U(0, 32 bits)
  )
}

object MduGen {
  def main(args: Array[String]): Unit = {
    val spinalConfig = SpinalConfig(
      targetDirectory = "hw/gen",
      defaultConfigForClockDomains = J1cpuConfig().clockConfig
    )
    spinalConfig.generateVerilog(new Mdu())
  }
}