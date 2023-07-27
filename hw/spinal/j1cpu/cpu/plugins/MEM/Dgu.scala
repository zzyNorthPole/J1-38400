package j1cpu.cpu.plugins.MEM

import j1cpu.cpu.signals.MemOp
import spinal.core._

class Dgu extends Component {
  val io = new Bundle {
    val addr = in UInt (32 bits)
    val memOp = in(MemOp())
    val din1, din2 = in UInt (32 bits)
    val dout = out UInt (32 bits)
  }
  noIoPrefix()

  import io._
  dout := memOp.mux(
    MemOp.B -> addr(1 downto 0).muxListDc(
      for (i <- 0 until 4) yield (i, B(24 bits, default -> din1(i * 8 + 7)).asUInt @@ din1((i * 8 + 7) downto (i * 8)))
    ),
    MemOp.BU -> addr(1 downto 0).muxListDc(
      for (i <- 0 until 4) yield (i, U(0, 24 bits) @@ din1((i * 8 + 7) downto (i * 8)))
    ),
    MemOp.H -> addr(1 downto 1).muxListDc(
      for (i <- 0 until 2) yield (i, B(16 bits, default -> din1(i * 16 + 15)).asUInt @@ din1((i * 16 + 15) downto (i * 16)))
    ),
    MemOp.HU -> addr(1 downto 1).muxListDc(
      for (i <- 0 until 2) yield (i, U(0, 16 bits) @@ din1((i * 16 + 15) downto (i * 16)))
    ),
    MemOp.W -> din1,
    MemOp.WL -> addr(1 downto 0).muxListDc(
      for (i <- 0 until 4) yield (i, if (i == 3) din1 else din1((i * 8 + 7) downto 0) @@ din2(((3 - i) * 8 - 1) downto 0))
    ),
    MemOp.WR -> addr(1 downto 0).muxListDc(
      for (i <- 0 until 4) yield (i, if (i == 0) din1 else din2(31 downto ((4 - i) * 8)) @@ din1(31 downto (i * 8)))
    )
  )
}
