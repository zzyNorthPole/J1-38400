package j1cpu.cpu

import j1cpu.cpu.vexriscv.Pipeline
import j1cpu.cpu.plugins._
import j1cpu.cpu.plugins.id.Decoder
import j1cpu.cpu.plugins.ex.ExDescription
import j1cpu.cpu.plugins.mem.Mem
import spinal.core._

class J1cpu(val signal: J1cpuSignal) extends Component with Pipeline {
  type T = J1cpu

  import signal._

  val IF1 = newStage()
  val IF2 = newStage()
  val ID = newStage()
  val EX = newStage()
  val MEM1 = newStage()
  val MEM2 = newStage()
  val WB = newStage()

  plugins ++= List(
    new Decoder(),
    new ExDescription(),
    new Mem()
  ).filter(_ != null)


}
