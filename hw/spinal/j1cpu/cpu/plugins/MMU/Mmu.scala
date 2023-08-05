package j1cpu.cpu.plugins.MMU

import j1cpu.cpu.TlbConfig
import spinal.core.internals.Operator.UInt
import spinal.core._
import spinal.lib.MuxOH

class Mmu(config: TlbConfig) extends Component {
  val io = new Bundle {
    val en = in Bool()
    val w = in Bool()
    val virtAddr = in UInt (32 bits)
    val phyAddr = out UInt (32 bits)
    val k0Cached = in Bool()
    val cached = out Bool()

    val tlb = new Bundle {
      val VPN = out UInt(20 bits)
      val din = in(new TLBResult)
      val hit = in Bool()
    }

    val tlbException = new Bundle {
      val refill = out Bool()
      val invalid = out Bool()
      val modified = out Bool()
    }
  }
  noIoPrefix()

  import io._

  val virtual = new Bundle {
    val useg = U"32'h00000000" // user mapped
    val kseg0 = U"32'h80000000" // kernel unmapped
    val kseg1 = U"32'hA0000000" // kernel unmapped uncached
    val ksseg = U"32'hC0000000" // supervisor mapped
    val kseg3 = U"32'hE0000000" // kernel mapped
  }
  val physical = new Bundle {
    val useg = U"32'h00000000"
    val kseg0 = U"32'h00000000"
    val kseg1 = U"32'h00000000"
    val ksseg = U"32'hC0000000"
    val kseg3 = U"32'hE0000000"
  }
  val unmapped = virtAddr >= virtual.kseg0 && virtAddr < virtual.kseg1
  val unmappedUncached = virtAddr >= virtual.kseg1 && virtAddr < virtual.ksseg
  val mapped = ~unmapped && ~unmappedUncached
  phyAddr := MuxOH(
    Vec(
      unmapped,
      unmappedUncached,
      mapped
    ),
    Vec(
      virtAddr - virtual.kseg0 + physical.kseg0,
      virtAddr - virtual.kseg1 + physical.kseg1,
      if (config.use) (tlb.din.PFN @@ virtAddr(11 downto 0)) else virtAddr
    )
  )
  cached := MuxOH(
    Vec(
      unmapped,
      unmappedUncached,
      mapped
    ),
    Vec(
      k0Cached,
      False,
      tlb.din.C === U(3, 3 bits)
    )
  )
  tlb.VPN := (if (config.use) virtAddr(31 downto 12) else U(0, 20 bits))
  tlbException.refill := en && MuxOH(
    Vec(
      unmapped,
      unmappedUncached,
      mapped
    ),
    Vec(
      False,
      False,
      if (config.use) !tlb.hit else False
    )
  )
  tlbException.invalid := en && MuxOH(
    Vec(
      unmapped,
      unmappedUncached,
      mapped
    ),
    Vec(
      False,
      False,
      if (config.use) (tlb.hit && !tlb.din.V) else False
    )
  )
  tlbException.modified := en && w && MuxOH(
    Vec(
      unmapped,
      unmappedUncached,
      mapped
    ),
    Vec(
      False,
      False,
      if (config.use) (tlb.hit && tlb.din.V && !tlb.din.D) else False
    )
  )
}
