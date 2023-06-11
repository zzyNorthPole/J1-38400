package j1cpu.cpu.plugins.id

import j1cpu.cpu.J1cpu
import j1cpu.cpu.vexriscv.{Pipeline, Plugin}
import spinal.core._

class Decoder extends Plugin[J1cpu] {
  override def build(pipeline: J1cpu): Unit = {
    import pipeline._
    import pipeline.signal._


  }
}
