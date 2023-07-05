package j1cpu.cpu.plugins.IF

import spinal.core._
import spinal.lib._

class PcManager extends Component {
  val io = new Bundle {
    val isStalled = in Bool()
    val isExceptionFlushed = in Bool()
    val isBranchPredictErrorFlushed = in Bool()
    val branchPredictCorrectAddr = in UInt (32 bits)
    val pc = out UInt (32 bits)
  }
  noIoPrefix()
  import io._

  val pcReg = RegInit(U"32'hBFC00000")
  // if it is flushed by exception, ignore branch predict error and set pcReg
  // flushed signal forced to change pcReg ignoring stall signal
  // flush but not stall will discard previous IF1 inst using iCache pipeline control signal
  // flush and stall won't have valid IF1 inst
  when(!isStalled) {
    pcReg := pcReg + 4
  }
  when(isBranchPredictErrorFlushed) {
    pcReg := branchPredictCorrectAddr
  }
  when(isExceptionFlushed) {
    pcReg := U"32'hBFC00000"
  }
  pc := pcReg
}
