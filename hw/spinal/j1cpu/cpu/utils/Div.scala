package j1cpu.cpu.utils

import j1cpu.cpu.J1cpuConfig
import spinal.core._
import spinal.lib._
import spinal.lib.fsm.{State, StateMachine}

class Div(width: Int) extends Component {
  val io = new Bundle {
    val en = in Bool()
    val signed = in Bool()
    val din1, din2 = in UInt (width bits) // din1 / din2
    val dout1, dout2 = out UInt (width bits) // din1 = din2 * dout1 + dout2
    val ready = out Bool()
  }
  noIoPrefix()
  import io._
  val signDin1, signDin2 = Bool()
  signDin1 := din1(width - 1)
  signDin2 := din2(width - 1)
  val absDin1, absDin2 = UInt(width bits)
  absDin1 := signed ? din1.asSInt.abs | din1
  absDin2 := signed ? din2.asSInt.abs | din2

  val reLife = RegInit(False)
  val count = RegInit(U(0, log2Up(width) bits))
  val P = RegInit(U(0, (width + 1) bits))
  val Q = RegInit(U(0, width bits))
  val curP, nextP = UInt ((width + 1) bits)
  val nextQ = UInt (width bits)
  curP := P(width - 1 downto 0) @@ Q(width - 1 downto width - 1)
  nextQ := Q(width - 2 downto 0) @@ U(0, 1 bits)
  val remain = UInt((width + 1) bits)
  remain := curP - (U(0, 1 bits) @@ absDin2)
  nextP := remain(width) ? curP | remain
  val absDout1, absDout2 = RegInit(U(0, width bits))
  val divFSM = new StateMachine {
    setEntry(stateBoot)
    disableAutoStart()

    val execute = new State()

    stateBoot.whenIsActive {
      when(reLife) {
        io.ready := True
      }.otherwise {
        io.ready := !en
      }

      when(!io.ready) {
        reLife := True
        when(en) {
          count := 0
          P := 0
          Q := absDin1
          absDout1 := 0
          absDout2 := 0
          goto(execute)
        }
      }.otherwise {
        reLife := False
      }
    }

    execute.whenIsActive {
      io.ready := False
      count := count + 1
      P := nextP
      Q := nextQ
      absDout1(width - 1 - count) := !remain(width)
      absDout2 := nextP((width - 1) downto 0)
      when(count === U(width - 1, log2Up(width) bits)) {
        goto(stateBoot)
      }
    }
  }

  val signDout1, signDout2 = Bool()
  signDout1 := signDin1 ^ signDin2
  signDout2 := signDin1
  dout1 := signed ? (
    signDout1 ? (U(0, width bits) - absDout1) | absDout1
  ) | absDout1
  dout2 := signed ? (
    signDout2 ? (U(0, width bits) - absDout2) | absDout2
  ) | absDout2
}

object DivGen {
  def main(args: Array[String]): Unit = {
    val spinalConfig = SpinalConfig(
      targetDirectory = "hw/gen",
      defaultConfigForClockDomains = J1cpuConfig().clockConfig
    )
    spinalConfig.generateVerilog(new Div(32))
  }
}