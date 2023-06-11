package j1cpu.axi

import j1cpu.cpu.J1cpuConfig
import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axi.{Axi4, Axi4Config}

class AxiCrossbar(axiConfig: Axi4Config) extends Component {
  val io = new Bundle {
    val iBus, dBus, udBus = slave(Axi4(axiConfig)).setBlocked()
    val cpuBus = master(Axi4(axiConfig)).setIdle()
  }
  noIoPrefix()

  // in this axiCrossbar, we separate ar/r and aw/w/b

  // when two bus's req arrived at the same time,
  // dbus/udbus > ibus as verilog's properties

  // when a bus is firing, (record it as 0 clock)
  // 1 clock will set busy to 0, and 2 clock will arbitrate new request

  // but we shouldn't concern about it,
  // as what we need to concentrate on the different behavior in operate on the three bus!

  // thanks for nscscc2022 Tsinghua University ZenCove's axiCrossbar

  import io._
  cpuBus.flattenForeach {
    signal => {
      val tmpName = signal.getName()
      val tmpList = tmpName.split("_")
      signal.setName(tmpList(1) + tmpList(tmpList.size - 1))
    }
  }

  val busList = List(iBus, dBus, udBus)
  for (bus <- busList) {
    bus.flattenForeach {
      signal => {
        val tmpName = signal.getName()
        val tmpList = tmpName.split("_")
        signal.setName(tmpList(0) + "_" + tmpList(1) + "_" + tmpList(tmpList.size - 1))
      }
    }
  }

  val readBus = List(iBus, dBus, udBus)
  val arReq = readBus.map(_.ar.valid).orR
  val rBusy = RegInit(False)
  val rArbitrate = RegInit(U"00")
  when(!rBusy && arReq) {
    rBusy := True
    for (i <- 0 to 2) {
      when(readBus(i).ar.valid === True) {
        rArbitrate := i
      }
    }
  }
  when(rBusy) {
    for (i <- 0 to 2) {
      when(rArbitrate === i) {
        readBus(i).ar >> cpuBus.ar
        readBus(i).r << cpuBus.r
      }
    }
  }
  when(cpuBus.r.fire) {
    rBusy := False
  }

  val writeBus = List(dBus, udBus)
  val awReq = writeBus.map(_.aw.valid).orR
  val wBusy = RegInit(False)
  val wArbitrate = Reg(U"0")
  when(!wBusy && awReq) {
    wBusy := True
    for (i <- 0 to 1) {
      when(writeBus(i).ar.valid === True) {
        wArbitrate := i
      }
    }
  }
  when(wBusy) {
    for (i <- 0 to 1) {
      when(wArbitrate === i) {
        writeBus(i).aw >> cpuBus.aw
        writeBus(i).w >> cpuBus.w
        writeBus(i).b << cpuBus.b
      }
    }
  }
  when(cpuBus.b.fire) {
    wBusy := False
  }
}

object AxiCrossbarGen {
  def main(args: Array[String]): Unit = {
    val spinalConfig = SpinalConfig(
      targetDirectory = "hw/gen",
      defaultConfigForClockDomains = (new J1cpuConfig).clockConfig
    )

    spinalConfig.generateVerilog(new AxiCrossbar((new J1cpuConfig()).axiConfig))
  }
}