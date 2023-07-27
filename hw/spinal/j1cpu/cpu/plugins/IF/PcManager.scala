package j1cpu.cpu.plugins.IF

import spinal.core._
import spinal.lib._

class PcManager extends Component {
  val io = new Bundle {
    val isValid = in Bool()
    val isStalled = in Bool()
    val isExceptionFlushed = in Bool()
    val exceptionTrapAddr = in UInt (32 bits)
    val isTLBFlushed = in Bool()
    val tlbInstructionNextAddr = in UInt(32 bits)
    val isBranchPredictErrorFlushed = in Bool()
    val branchPredictCorrectAddr = in UInt (32 bits)
    val isBranchPredict = in Bool()
    val branchPredictAddr = in UInt (32 bits)
    val pc = out UInt (32 bits)
    val addressErrorLoad = out Bool()
  }
  noIoPrefix()
  import io._

  val pcReg = RegInit(U"32'hBFC00000")
  // flush logic:
  // if it is flushed by exception, ignore branch predict error and set pcReg
  // flushed signal forced to change pcReg ignoring stall signal
  // flush but not stall will discard previous IF1 inst using iCache pipeline control signal
  // flush and stall won't have valid IF1 inst
  // in the common situation:
  // find the branch predict result in IF2 stage
  // the result put forward into pcReg when IF2 stage inst is moving
  // at this time IF2 stage move into ID stage
  // and IF1 stage delay slot move into IF2 stage (maybe wait for inst data)
  when(isExceptionFlushed) {
    pcReg := exceptionTrapAddr
  }.elsewhen(isTLBFlushed) {
    pcReg := tlbInstructionNextAddr
  }.elsewhen(isBranchPredictErrorFlushed) {
    pcReg := branchPredictCorrectAddr
  }.elsewhen(!isStalled) {
    when(isBranchPredict) {
      pcReg := branchPredictAddr
    }.elsewhen(isValid) {
      pcReg := pcReg + 4
    }
  }
  pc := pcReg
  addressErrorLoad := isValid && (pc(1 downto 0) =/= U(0, 2 bits))
}
