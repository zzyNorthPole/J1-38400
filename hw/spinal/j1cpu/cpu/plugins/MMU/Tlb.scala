package j1cpu.cpu.plugins.MMU

import j1cpu.cpu.{J1cpuConfig, TlbConfig}
import spinal.core._
import spinal.lib._

// mips32 release 1 only support 4kb page
class TLBRequest extends Bundle {
  val VPN = UInt(20 bits)
  val ASID = UInt(8 bits)
}
class TLBResult extends Bundle {
  val PFN = UInt(20 bits)
  val C = UInt(3 bits)
  val D = Bool()
  val V = Bool()
}
class TLBItem extends Bundle {
  val VPN2 = UInt(19 bits)
  val ASID = UInt(8 bits)
  val G = Bool()
  val PFN0, PFN1 = UInt(20 bits)
  val C0, C1 = UInt(3 bits)
  val D0, D1 = Bool()
  val V0, V1 = Bool()
}
class TLB(config: TlbConfig) extends Component {
  val io = new Bundle {
    val wPorts = new Bundle {
      val en = in Bool()
      val addr = in UInt(log2Up(config.lines) bits)
      val din = in(new TLBItem)
    }
    val rPorts = new Bundle {
      val addr = in UInt(log2Up(config.lines) bits)
      val dout = out(new TLBItem)
    }
    val queryPorts = Vec(
      new Bundle {
        val din = in(new TLBRequest)
        val hit = out Bool()
        val addr = out UInt(log2Up(config.lines) bits)
        val dout = out(new TLBResult)
      },
      2
    )
  }
  noIoPrefix()

  import io._

  if (config.use) {
    val tlb = Vec(Reg(new TLBItem), config.lines)
    for (i <- 0 until config.lines) {
      tlb(i).VPN2 init U(19 bits, default -> True)
      tlb(i).ASID init U(0, 8 bits)
      tlb(i).G init False
      tlb(i).PFN0 init U(0, 20 bits)
      tlb(i).PFN1 init U(0, 20 bits)
      tlb(i).C0 init U(0, 3 bits)
      tlb(i).C1 init U(0, 3 bits)
      tlb(i).D0 init False
      tlb(i).D1 init False
      tlb(i).V0 init False
      tlb(i).V1 init False
    }
    val hits = Vec(Vec(Bool(), config.lines), 2)

    for (i <- 0 until 2) {
      for (j <- 0 until config.lines) {
        hits(i)(j) := (tlb(j).VPN2 === queryPorts(i).din.VPN(19 downto 1)) && ((tlb(j).ASID === queryPorts(i).din.ASID) || tlb(j).G)
      }
      queryPorts(i).hit := hits(i).orR
    }

    for (i <- 0 until 2) {
      queryPorts(i).addr := MuxOH(
        hits(i),
        for (j <- 0 until config.lines) yield U(j, log2Up(config.lines) bits)
      )
      queryPorts(i).dout.PFN := MuxOH(
        hits(i),
        for (j <- 0 until config.lines) yield (queryPorts(i).din.VPN(0) ? tlb(j).PFN1 | tlb(j).PFN0)
      )
      queryPorts(i).dout.C := MuxOH(
        hits(i),
        for (j <- 0 until config.lines) yield (queryPorts(i).din.VPN(0) ? tlb(j).C1 | tlb(j).C0)
      )
      queryPorts(i).dout.D := MuxOH(
        hits(i),
        for (j <- 0 until config.lines) yield (queryPorts(i).din.VPN(0) ? tlb(j).D1 | tlb(j).D0)
      )
      queryPorts(i).dout.V := MuxOH(
        hits(i),
        for (j <- 0 until config.lines) yield (queryPorts(i).din.VPN(0) ? tlb(j).V1 | tlb(j).V0)
      )
    }

    when(wPorts.en) {
      tlb(wPorts.addr) := wPorts.din
    }

    rPorts.dout := tlb(rPorts.addr)
  }
  else {
    rPorts.dout.VPN2 := U(0, 19 bits)
    rPorts.dout.ASID := U(0, 8 bits)
    rPorts.dout.G := False
    rPorts.dout.PFN0 := U(0, 20 bits)
    rPorts.dout.PFN1 := U(0, 20 bits)
    rPorts.dout.C0 := U(0, 3 bits)
    rPorts.dout.C1 := U(0, 3 bits)
    rPorts.dout.D0 := False
    rPorts.dout.D1 := False
    rPorts.dout.V0 := False
    rPorts.dout.V1 := False
    for (i <- 0 to 1) {
      queryPorts(i).hit := True
      queryPorts(i).addr := U(0, log2Up(config.lines) bits)
      queryPorts(i).dout.PFN := U(0, 20 bits)
      queryPorts(i).dout.C := U(0, 3 bits)
      queryPorts(i).dout.D := False
      queryPorts(i).dout.V := False
    }
  }
}

object TLBGen {
  def main(args: Array[String]): Unit = {
    val spinalConfig = SpinalConfig(
      targetDirectory = "hw/gen",
      defaultConfigForClockDomains = J1cpuConfig().clockConfig
    )
    spinalConfig.generateVerilog(new TLB(TlbConfig(true, 8)))
  }
}