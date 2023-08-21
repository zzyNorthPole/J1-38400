package j1cpu

import j1cpu.axi.AxiCrossbar
import j1cpu.cpu.plugins.MEM.MemDescription
import j1cpu.cpu.plugins.WB.WbDescription
import j1cpu.cpu.signals.Exception
import j1cpu.cpu.{J1cpu, J1cpuConfig, J1cpuSignal}
import spinal.core._
import spinal.lib.bus.amba4.axi.Axi4
import spinal.lib.{clockDomainPimped, master}

class MyCpuTop extends Component {
  setDefinitionName("mycpu_top")
  val io = new Bundle {
    val aclk = in Bool()
    val aresetn = in Bool()

    val ext_int = in Bits(6 bits)
    val wid = out UInt(4 bits)
    val cpuBus = master(Axi4(J1cpuConfig().axiConfig))
    val arlock = out Bits(2 bits)
    val awlock = out Bits(2 bits)
    val debug = new Bundle {
      val wb = new Bundle {
        val pc = out UInt (32 bits)
        val rf = new Bundle {
          val wen = out Bits (4 bits)
          val wnum = out UInt (5 bits)
          val wdata = out UInt (32 bits)
        }
      }
      val cp0 = new Bundle {
        val random = out UInt(32 bits)
        val count = out UInt(32 bits)
        val cause = out UInt(32 bits)
      }
      val int = out Bool()
      val commit = out Bool()
    }
  }
  noIoPrefix()
  import io._

  cpuBus.flattenForeach {
    signal => {
      val tmpName = signal.getName()
      val tmpList = tmpName.split("_")
      signal.setName(tmpList(1) + tmpList(tmpList.size - 1))
    }
  }

  val clk = Bool()
  clk := io.aclk
  val reset = Bool()
  reset := ~io.aresetn
  wid := U(1, 4 bits)
  val defaultClockDomain = ClockDomain(
    clock = clk,
    reset = reset,
    config = ClockDomainConfig(
      resetActiveLevel = HIGH,
      resetKind = SYNC,
      clockEdge = RISING
    )
  )
  val defaultClockArea = new ClockingArea(defaultClockDomain) {
    val cpu = new J1cpu(J1cpuConfig(), new J1cpuSignal())
    val axiCrossbar = new AxiCrossbar(J1cpuConfig().axiConfig)

    cpu.io.iBus.toAxi4() >> axiCrossbar.io.iBus
    cpu.io.dBus >> axiCrossbar.io.dBus
    cpu.io.udBus >> axiCrossbar.io.udBus
    arlock := B(0, 2 bits)
    awlock := B(0, 2 bits)

    axiCrossbar.io.cpuBus >> cpuBus

    cpu.io.extInt := ext_int

    debug.wb.pc := cpu.service[WbDescription].debug.pc
    debug.wb.rf.wen := cpu.service[WbDescription].debug.rfWen
    debug.wb.rf.wnum := cpu.service[WbDescription].debug.rfWnum
    debug.wb.rf.wdata := cpu.service[WbDescription].debug.rfWdata
    debug.cp0.count := cpu.service[WbDescription].debug.cp0Count
    debug.cp0.random := cpu.service[WbDescription].debug.cp0Random
    debug.cp0.cause := cpu.service[WbDescription].debug.cp0Cause
    debug.int := cpu.service[WbDescription].debug.int
    debug.commit := cpu.service[WbDescription].debug.commit
  }
}

object TopGen {
  def main(args: Array[String]): Unit = {
    val spinalConfig = SpinalConfig(
      targetDirectory = "hw/gen",
      defaultConfigForClockDomains = ClockDomainConfig(
        resetActiveLevel = HIGH,
        resetKind = SYNC,
        clockEdge = RISING
      )
    )

    spinalConfig.generateVerilog(new MyCpuTop()).printPruned()
  }
}
