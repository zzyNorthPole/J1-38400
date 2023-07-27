package j1cpu.cpu.plugins.EX

import j1cpu.cpu.J1cpuConfig
import j1cpu.cpu.signals.MemOp
import spinal.core._
import spinal.lib._

class Agu extends Component {
  val io = new Bundle {
    val en = in Bool()
    val memW = in Bool()
    val memOp = in(MemOp())
    val memWe = out Bits (4 bits)
    val din1, din2 = in UInt (32 bits)
    val addr = out UInt (32 bits)
    val din = in UInt (32 bits)
    val dout = out UInt (32 bits)
    val addressErrorLoad = out Bool()
    val addressErrorStore = out Bool()
  }
  noIoPrefix()
  import io._
  addr := din1 + din2
  memWe := (
    !memW ? B"0000" | memOp.mux(
      MemOp.B -> addr(1 downto 0).muxListDc(
        for (i <- 0 until 4) yield (i, B(4 bits, i -> True, default -> False))
      ),
      MemOp.H -> (addr(1) ? B"1100" | B"0011"),
      MemOp.W -> B"1111",
      MemOp.WL -> addr(1 downto 0).muxListDc(
        for (i <- 0 until 4) yield (i, B(4 bits, (i downto 0) -> True, default -> False))
      ),
      MemOp.WR -> addr(1 downto 0).muxListDc(
        for (i <- 0 until 4) yield (i, B(4 bits, (3 downto i) -> True, default -> False))
      ),
      default -> B"0000"
    )
  )
  dout := (
    !memW ? din | memOp.mux(
      MemOp.B -> din(7 downto 0) @@ din(7 downto 0) @@ din(7 downto 0) @@ din(7 downto 0),
      MemOp.H -> din(15 downto 0) @@ din(15 downto 0),
      MemOp.W -> din,
      MemOp.WL -> addr(1 downto 0).muxListDc(
        for (i <- 0 until 4) yield (i, U(0, 8 * (3 - i) bits) @@ din(31 downto (8 * (3 - i))))
      ),
      MemOp.WR -> addr(1 downto 0).muxListDc(
        for (i <- 0 until 4) yield (i, din(((4 - i) * 8 - 1) downto 0) @@ U(0, i * 8 bits))
      ),
      default -> U(0, 32 bits)
    )
  )

  addressErrorLoad := en && !memW && (
    ((memOp === MemOp.H || memOp === MemOp.HU) && addr(0)) ||
      (memOp === MemOp.W && addr(1 downto 0) =/= U(0, 2 bits))
  )
  addressErrorStore := en && memW && (
    (memOp === MemOp.H && addr(0)) ||
      (memOp === MemOp.W && addr(1 downto 0) =/= U(0, 2 bits))
  )
}

object AguGen {
  def main(args: Array[String]): Unit = {
    val spinalConfig = SpinalConfig(
      targetDirectory = "hw/gen",
      defaultConfigForClockDomains = J1cpuConfig().clockConfig
    )
    spinalConfig.generateVerilog(new Agu())
  }
}