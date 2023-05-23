package j1cpu.cpu.plugins.mem

import j1cpu.cpu.J1cpu
import j1cpu.cpu.vexriscv.Plugin
import spinal.core._

class Mem extends Plugin[J1cpu] {
  override def build(pipeline: J1cpu): Unit = {
    import pipeline._
    import pipeline.signal._


  }
}
