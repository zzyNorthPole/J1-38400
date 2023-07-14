package j1cpu.cpu.plugins.EX

import spinal.core._
import spinal.lib._

class Agu extends Component {
  val io = new Bundle {
    val din1, din2 = in UInt (32 bits)
    val addr = out UInt (32 bits)
  }
  noIoPrefix()
  import io._
  addr := din1 + din2
}
