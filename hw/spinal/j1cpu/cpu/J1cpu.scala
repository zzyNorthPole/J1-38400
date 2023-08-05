package j1cpu.cpu

import j1cpu.cpu.plugins.EX.ExDescription
import j1cpu.cpu.plugins.ID.IdDescription
import j1cpu.cpu.plugins.IF.IfDescription
import j1cpu.cpu.plugins.MEM.MemDescription
import j1cpu.cpu.plugins.WB.WbDescription
import j1cpu.cpu.vexriscv.Pipeline
import j1cpu.cpu.plugins._
import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axi.{Axi4, Axi4ReadOnly}

class J1cpu(config: J1cpuConfig, val signal: J1cpuSignal) extends Component with Pipeline {
  type T = J1cpu

  val io = new Bundle {
    val extInt = in Bits (5 bits)
    val iBus = master(Axi4ReadOnly(config.axiConfig))
    val dBus = master(Axi4(config.axiConfig))
    val udBus = master(Axi4(config.axiConfig))
  }
  noIoPrefix()
  import io._

  import signal._

  import io._

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

  val IF1 = newStage()
  val IF2 = newStage()
  val ID = newStage()
  val EX = newStage()
  val MEM1 = newStage()
  val MEM2 = newStage()
  val WB = newStage()

  plugins ++= List(
    new IfDescription(config),
    new IdDescription(config),
    new ExDescription(),
    new MemDescription(config),
    new WbDescription()
  ).filter(_ != null)

  service[IfDescription].iCache.io.ibus <> iBus

  service[MemDescription].dCache.io.dbus <> dBus
  service[MemDescription].dCache.io.udbus <> udBus
  service[MemDescription].cp0.io.extInt := io.extInt
}