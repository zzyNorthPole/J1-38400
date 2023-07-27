package j1cpu.cpu.plugins.MEM

import j1cpu.cpu.J1cpuConfig
import j1cpu.cpu.plugins.MMU.{TLBItem, TLBRequest}
import j1cpu.cpu.signals.{Cp0Reg, Exception}
import j1cpu.cpu.utils.Lfsr
import spinal.core._
import spinal.lib.MuxOH

class Cp0(config: J1cpuConfig) extends Component {
  val io = new Bundle {
    // exception
    val ex = in Bool()
    val exOp = in(Exception())
    val tlbHit = in Bool() // 0 for TLB refill, 1 for TLB invalid
    val badPc = in UInt (32 bits)
    val delaySlot = in Bool()
    val badAddr = in UInt (32 bits)
    val badTLBRequest = in(new TLBRequest)
    val trapPc = out UInt (32 bits)

    // interrupt
    val extInt = in Bits(6 bits)
    val interrupt = out Bool()

    // eret
    val eret = in Bool()

    // mfc0 && mtc0
    val en = in Bool()
    val addr = in(Cp0Reg())
    val select = in UInt (3 bits)
    val din = in UInt (32 bits)
    val dout = out UInt (32 bits)

    // TLB and TLB inst
    val k0Cached = out Bool()
    val tlbp = in Bool()
    val tlbpHit = in Bool()
    val tlbpDin = in UInt (log2Up(config.tlbConfig.lines) bits)
    val tlbpDout = out(new TLBRequest)
    val tlbr = in Bool()
    val tlbrAddr = out UInt(log2Up(config.tlbConfig.lines) bits)
    val tlbrDin = in(new TLBItem)
    val tlbw = in Bool() // 1 for tlbwr 0 for tlbwi
    val tlbwAddr = out UInt(log2Up(config.tlbConfig.lines) bits)
    val tlbwDout = out(new TLBItem)

    val debug = new Bundle {
      val count = out UInt(32 bits)
      val random = out UInt(32 bits)
      val cause = out UInt(32 bits)
    }
  }
  noIoPrefix()

  import io._

  // cp0 reg 0, select 0
  val Index = new Bundle {
    val P = RegInit(U(0, 1 bits))
    val Index = RegInit(U(0, log2Up(config.tlbConfig.lines) bits))
  }
  val index = UInt(32 bits)
  index := Index.P @@ U(0, 32 - log2Up(config.tlbConfig.lines) - 1 bits) @@ Index.Index

  // cp0 reg 1, select 0
  val Random = new Bundle {
    val Random = RegInit(U(log2Up(config.tlbConfig.lines) bits, default -> True))
  }
  val random = UInt(32 bits)
  random := U(0, 32 - log2Up(config.tlbConfig.lines) bits) @@ Random.Random

  // cp0 reg 2, select 0
  // cp0 reg 3, select 0
  val EntryLo0, EntryLo1 = new Bundle {
    val Fill = U(0, 6 bits)
    val PFN = RegInit(U(0, 20 bits))
    val C = RegInit(U(0, 3 bits))
    val D = RegInit(U(0, 1 bits))
    val V = RegInit(U(0, 1 bits))
    val G = RegInit(U(0, 1 bits))
  }
  val entryLo0 = UInt(32 bits)
  val entryLo1 = UInt(32 bits)
  entryLo0 := EntryLo0.Fill @@ EntryLo0.PFN @@ EntryLo0.C @@ EntryLo0.D @@ EntryLo0.V @@ EntryLo0.G
  entryLo1 := EntryLo1.Fill @@ EntryLo1.PFN @@ EntryLo1.C @@ EntryLo1.D @@ EntryLo1.V @@ EntryLo1.G

  // cp0 reg 4, select 0
  val Context = new Bundle {
    val PTEBase = RegInit(U(0, 9 bits))
    val BadVPN2 = RegInit(U(0, 19 bits))
  }
  val context = UInt(32 bits)
  context := Context.PTEBase @@ Context.BadVPN2 @@ U(0, 4 bits)

  // cp0 reg 5, select 0
  val PageMask = new Bundle {
    val Mask = U(0, 16 bits)
  }
  val pageMask = UInt(32 bits)
  pageMask := U(0, 3 bits) @@ PageMask.Mask @@ U(0, 13 bits)

  // cp0 reg 6, select 0
  val Wired = new Bundle {
    val Wired = RegInit(U(0, log2Up(config.tlbConfig.lines) bits))
  }
  val wired = UInt(32 bits)
  wired := U(0, 32 - log2Up(config.tlbConfig.lines) bits) @@ Wired.Wired

  // cp0 reg 8, select 0
  val BadVAddr = new Bundle {
    val BadVAddr = RegInit(U(0, 32 bits))
  }
  val badVAddr = UInt(32 bits)
  badVAddr := BadVAddr.BadVAddr

  // cp0 reg 9, select 0
  val Count = new Bundle {
    val Count = RegInit(U(0, 32 bits))
  }
  val count = UInt(32 bits)
  count := Count.Count

  // cp0 reg 10, select 0
  val EntryHi = new Bundle {
    val VPN2 = RegInit(U(0, 19 bits))
    val ASID = RegInit(U(0, 8 bits))
  }
  val entryHi = UInt(32 bits)
  entryHi := EntryHi.VPN2 @@ U(0, 5 bits) @@ EntryHi.ASID

  // cp0 reg 11, select 0
  val Compare = new Bundle {
    val Compare = RegInit(U(0, 32 bits))
  }
  val compare = UInt(32 bits)
  compare := Compare.Compare

  // cp0 reg 12, select 0
  val Status = new Bundle {
    val CU = RegInit(U(0, 4 bits))
    val BEV = RegInit(U(1, 1 bits))
    val IM = RegInit(U(0, 8 bits))
    val UM = RegInit(U(0, 1 bits))
    val ERL = RegInit(U(0, 1 bits)) // optional
    val EXL = RegInit(U(0, 1 bits))
    val IE = RegInit(U(0, 1 bits))
  }
  val status = UInt(32 bits)
  status :=
    Status.CU @@ U(0, 5 bits) @@ Status.BEV @@ U(0, 6 bits) @@ Status.IM @@
      U(0, 3 bits) @@ Status.UM @@ U(0, 1 bits) @@ Status.ERL @@ Status.EXL @@ Status.IE

  // cp0 reg 13, select 0
  val Cause = new Bundle {
    val BD = RegInit(U(0, 1 bits))
    val IV = RegInit(U(0, 1 bits)) // optional
    val IP = RegInit(U(0, 8 bits))
    val Exc = RegInit(U(0, 5 bits))
  }
  val cause = UInt(32 bits)
  cause := Cause.BD @@ U(0, 7 bits) @@ Cause.IV @@ U(0, 7 bits) @@ Cause.IP @@
    U(0, 1 bits) @@ Cause.Exc @@ U(0, 2 bits)

  // cp0 reg 14, select 0
  val EPC = new Bundle {
    val EPC = RegInit(U(0, 32 bits))
  }
  val epc = UInt(32 bits)
  epc := EPC.EPC

  // cp0 reg 15, select 0
  val PRId = new Bundle {
    val PRId = U"32'h00007000"
  }
  val prid = UInt(32 bits)
  prid := PRId.PRId

  // cp0 reg 15, select 1
  val EBase = new Bundle {
    val EBase = U"32'h80000000"
  }
  val eBase = UInt(32 bits)
  eBase := EBase.EBase

  // cp0 reg 16, select 0
  val Config0 = new Bundle {
    val M = U(1, 1 bits) // 1 denotes config1 register is implemented
    val BE = U(0, 1 bits) // 0 for little endian
    val AT = U(0, 2 bits) // 0 for MIPS32
    val AR = U(0, 3 bits) // 0 for MIPS32 release 1
    val MT = U(1, 3 bits) // 1 for standard TLB
    val VI = U(0, 1 bits) // 0 denotes instruction cache is not virtual(using both virtual indexing and virtual tags)
    val K0 = RegInit(U(3, 3 bits)) // kseg0 cacheability and coherency attribute
  }
  val config0 = UInt(32 bits)
  config0 := Config0.M @@ U(0, 15 bits) @@ Config0.BE @@ Config0.AT @@
    Config0.AR @@ Config0.MT @@ U(0, 3 bits) @@ Config0.VI @@ Config0.K0

  // cp0 reg 16, select 1
  val Config1 = new Bundle {
    val M = U(0, 1 bits) // 0 denotes config 2 register is not implemented
    val MMUSize = U(config.tlbConfig.lines - 1, 6 bits) // number of entries in the tlb
    val IS = U(log2Up(config.cacheConfig.lines) - 6, 3 bits) // icache sets per way: 0-64
    val IL = U(log2Up(config.cacheConfig.blockSize) - 1, 3 bits) // icache line size: 1-4 bytes
    val IA = U(config.cacheConfig.ways - 1, 3 bits) // icache associativity: 1-2 way
    val DS = U(log2Up(config.cacheConfig.lines) - 6, 3 bits) // dcache sets per way: 0-64
    val DL = U(log2Up(config.cacheConfig.blockSize) - 1, 3 bits) // dcache line size: 1-4 bytes
    val DA = U(config.cacheConfig.ways - 1, 3 bits) // dcache associativity: 1-2 way
    val C2 = U(0, 1 bits) // no coprocessor 2 implemented
    val MD = U(0, 1 bits)
    val PC = U(0, 1 bits) // no performance counter registers implemented
    val WR = U(0, 1 bits) // no watch registers implemented
    val CA = U(0, 1 bits) // mips163 not implemented
    val EP = U(0, 1 bits) // no EJTAG implemented
    val FP = U(0, 1 bits) // no FPU implemented
  }
  val config1 = UInt(32 bits)
  config1 := Config1.M @@ Config1.MMUSize @@
    Config1.IS @@ Config1.IL @@ Config1.IA @@
    Config1.DS @@ Config1.DL @@ Config1.DA @@
    Config1.C2 @@ Config1.MD @@ Config1.PC @@ Config1.WR @@ Config1.CA @@ Config1.EP @@ Config1.FP

  // cp0 reg 30, select 1 (optional)
  val ErrorEPC = new Bundle {
    val ErrorEPC = RegInit(U(0, 32 bits))
  }
  val errorEPC = UInt(32 bits)
  errorEPC := ErrorEPC.ErrorEPC

  // cp0 reg 0, select 0
  when(tlbp) {
    Index.P := tlbpHit.asUInt
    Index.Index := tlbpDin
  }.elsewhen(en && addr === Cp0Reg.Index) {
    Index.Index := din(log2Up(config.tlbConfig.lines) - 1 downto 0)
  }
  tlbrAddr := index(log2Up(config.tlbConfig.lines) - 1 downto 0)

  // cp0 reg 1, select 0
  val lfsrWidth = log2Up(config.tlbConfig.lines) + 2
  val lfsr = new Lfsr(lfsrWidth)
  lfsr.io.en := True
  lfsr.io.seed := U((lfsrWidth - 1 downto 1) -> False, 0 -> True)
  val lfsrDout = UInt(lfsrWidth bits)
  lfsrDout := lfsr.io.dout
  when(en && addr === Cp0Reg.Wired) {
    Random.Random := din(log2Up(config.tlbConfig.lines) - 1 downto 0)
  }.otherwise {
    when(Random.Random === U(log2Up(config.tlbConfig.lines) bits, default -> True)) {
      Random.Random := Wired.Wired
    }.otherwise {
      Random.Random := Random.Random + (U(0, log2Up(config.tlbConfig.lines) - 1 bits) @@ lfsrDout(1))
    }
  }
  tlbwAddr := (tlbw ? random | index)(log2Up(config.tlbConfig.lines) - 1 downto 0)

  // cp0 reg 2, select 0
  // cp0 reg 3, select 0
  when(tlbr) {
    EntryLo0.PFN := tlbrDin.PFN0
    EntryLo0.C := tlbrDin.C0
    EntryLo0.D := tlbrDin.D0.asUInt
    EntryLo0.V := tlbrDin.V0.asUInt
    EntryLo0.G := tlbrDin.G.asUInt
    EntryLo1.PFN := tlbrDin.PFN1
    EntryLo1.C := tlbrDin.C1
    EntryLo1.D := tlbrDin.D1.asUInt
    EntryLo1.V := tlbrDin.V1.asUInt
    EntryLo1.G := tlbrDin.G.asUInt
  }.otherwise {
    when(en && addr === Cp0Reg.EntryLo0) {
      EntryLo0.PFN := din(25 downto 6)
      EntryLo0.C := din(5 downto 3)
      EntryLo0.D := din(2 downto 2)
      EntryLo0.V := din(1 downto 1)
      EntryLo0.G := din(0 downto 0)
    }
    when(en && addr === Cp0Reg.EntryLo1) {
      EntryLo1.PFN := din(25 downto 6)
      EntryLo1.C := din(5 downto 3)
      EntryLo1.D := din(2 downto 2)
      EntryLo1.V := din(1 downto 1)
      EntryLo1.G := din(0 downto 0)
    }
  }
  tlbwDout.PFN0 := EntryLo0.PFN
  tlbwDout.C0 := EntryLo0.C
  tlbwDout.D0 := EntryLo0.D.asBool
  tlbwDout.V0 := EntryLo0.V.asBool
  tlbwDout.PFN1 := EntryLo1.PFN
  tlbwDout.C1 := EntryLo1.C
  tlbwDout.D1 := EntryLo1.D.asBool
  tlbwDout.V1 := EntryLo1.V.asBool
  tlbwDout.G := (EntryLo0.G & EntryLo1.G).asBool

  // cp0 reg 4, select 0
  when(ex && (exOp === Exception.TLBL || exOp === Exception.TLBS || exOp === Exception.MOD)) {
    Context.BadVPN2 := badTLBRequest.VPN(19 downto 1)
  }.elsewhen(en && addr === Cp0Reg.Context) {
    Context.PTEBase := din(31 downto 23)
  }

  // cp0 reg 6, select 0
  when(en && addr === Cp0Reg.Wired) {
    Wired.Wired := din(log2Up(config.tlbConfig.lines) - 1 downto 0)
  }

  // cp0 reg 8, select 0
  when(ex && (
    exOp === Exception.ADEL || exOp === Exception.ADES ||
      exOp === Exception.TLBL || exOp === Exception.TLBS || exOp === Exception.MOD
    )) {
    BadVAddr.BadVAddr := badAddr
  }

  // cp0 reg 9, select 0
  val tikiTaka = RegInit(False)
  tikiTaka := ~tikiTaka
  when(en && addr === Cp0Reg.Count) {
    Count.Count := din
  }.elsewhen(tikiTaka) {
    Count.Count := Count.Count + 1
  }

  // cp0 reg 10, select 0
  when(ex && (exOp === Exception.TLBL || exOp === Exception.TLBS || exOp === Exception.MOD)) {
    EntryHi.VPN2 := badTLBRequest.VPN(19 downto 1)
  }.elsewhen(tlbr) {
    EntryHi.VPN2 := tlbrDin.VPN2
    EntryHi.ASID := tlbrDin.ASID
  }.elsewhen(en && addr === Cp0Reg.EntryHi) {
    EntryHi.VPN2 := din(31 downto 13)
    EntryHi.ASID := din(7 downto 0)
  }
  tlbpDout.VPN := EntryHi.VPN2 @@ U(0, 1 bits)
  tlbpDout.ASID := EntryHi.ASID
  tlbwDout.VPN2 := EntryHi.VPN2
  tlbwDout.ASID := EntryHi.ASID

  // cp0 reg 11, select 0
  when(en && addr === Cp0Reg.Compare) {
    Compare.Compare := din
  }

  // cp0 reg 12, select 0
  when(ex) {
    Status.EXL := 1
  }.elsewhen(eret) {
    when(Status.ERL === U(1, 1 bits)) {
      Status.ERL := 0
    }.otherwise {
      Status.EXL := 0
    }
  }.elsewhen(en && addr === Cp0Reg.Status) {
    Status.CU := din(31 downto 28)
    Status.BEV := din(22 downto 22)
    Status.IM := din(15 downto 8)
    Status.UM := din(4 downto 4)
    Status.ERL := din(2 downto 2) // optional
    Status.EXL := din(1 downto 1)
    Status.IE := din(0 downto 0)
  }

  // cp0 reg 13, select 0
  when(ex && Status.EXL === U"1'b0") {
    Cause.BD := delaySlot.asUInt
  }
  Cause.IP(7) := extInt(5) | (count === compare)
  Cause.IP(6 downto 2) := extInt(4 downto 0).asUInt
  when(en && addr === Cp0Reg.Cause) {
    Cause.IV := din(23 downto 23)
    Cause.IP(1 downto 0) := din(9 downto 8)
  }
  when(ex) {
    Cause.Exc := U(0, 1 bits) @@ exOp.asBits.asUInt
  }

  // cp0 reg 14, select 0
  when(ex && Status.EXL === U(0, 1 bits)) {
    EPC.EPC := delaySlot ? (badPc - 4) | badPc
  }.elsewhen(en && addr === Cp0Reg.EPC) {
    EPC.EPC := din
  }

  // cp0 reg 16, select 0
  when(en && addr === Cp0Reg.Config && select === U(0, 3 bits)) {
    Config0.K0 := din(2 downto 0)
  }
  k0Cached := Config0.K0 === U(3, 3 bits)

  // cp0 reg 30, select 0
  when(en && addr === Cp0Reg.ErrorEPC && select === U(0, 3 bits)) {
    ErrorEPC.ErrorEPC := din
  }

  val tlbRefillException = (exOp === Exception.TLBL || exOp === Exception.TLBS) && !tlbHit
  val intException = (exOp === Exception.INT) && Cause.IV(0) && !Status.BEV(0)
  trapPc := (
    eret ? (
      (Status.ERL === U(1, 1 bits)) ? errorEPC | epc
    ) | (
      ((Status.BEV === U(1, 1 bits)) ? U"32'hBFC00200" | eBase) +
        MuxOH(
          Vec(
            ex && tlbRefillException,
            ex && intException,
            ex && ~tlbRefillException
          ),
          Vec(
            U(0, 32 bits),
            U"32'h00000200",
            U"32'h00000180"
          )
        )
    )
  )

  interrupt := (Status.IE === U(1, 1 bits)) && (Status.EXL === U(0, 1 bits)) && (Status.ERL === U(0, 1 bits)) && ((Status.IM & Cause.IP).orR === True)

  dout := addr.mux(
    Cp0Reg.Index -> index,
    Cp0Reg.Random -> random,
    Cp0Reg.EntryLo0 -> entryLo0,
    Cp0Reg.EntryLo1 -> entryLo1,
    Cp0Reg.Context -> context,
    Cp0Reg.PageMask -> pageMask,
    Cp0Reg.Wired -> wired,
    Cp0Reg.BadVAddr -> badVAddr,
    Cp0Reg.Count -> count,
    Cp0Reg.EntryHi -> entryHi,
    Cp0Reg.Compare -> compare,
    Cp0Reg.Status -> status,
    Cp0Reg.Cause -> cause,
    Cp0Reg.EPC -> epc,
    Cp0Reg.PRIdEBase -> ((select === U(0, 3 bits)) ? prid | eBase),
    Cp0Reg.Config -> ((select(2 downto 1) === U(0, 2 bits)) ? ((select(0) === True) ? config1 | config0) | U(0, 32 bits)),
    Cp0Reg.ErrorEPC -> errorEPC
  )

  debug.count := count
  debug.random := random
  debug.cause := cause
}

object Cp0Gen {
  def main(args: Array[String]): Unit = {
    val spinalConfig = SpinalConfig(
      targetDirectory = "hw/gen",
      defaultConfigForClockDomains = J1cpuConfig().clockConfig
    )

    spinalConfig.generateVerilog(new Cp0(J1cpuConfig()))
  }
}