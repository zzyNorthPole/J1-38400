package j1cpu.cpu.plugins.IF

import j1cpu.cpu.signals.BpOp
import spinal.core._
import spinal.lib._

class MicroDu extends Component {
  val io = new Bundle {
    val inst = in Bits (32 bits)
    val bpuEn = out Bool()
    val bpuOp = out(BpOp())
    val pc = in UInt (32 bits)
    val offset = out UInt (32 bits)
  }
  noIoPrefix()

  import io._

  val opcode = inst(31 downto 26)
  val rs = inst(25 downto 21)
  val rt = inst(20 downto 16)
  val rd = inst(15 downto 11)
  val sa = inst(10 downto 6)
  val func = inst(5 downto 0)
  val imm = inst(15 downto 0)
  val instIdx = inst(25 downto 0)

  val isSpecial = opcode === B(0, 6 bits)
  val isRegImm = opcode === B"000001"
  val isSaZero = sa === B(0, 5 bits)
  val isRsZero = rs === B(0, 5 bits)
  val isRtZero = rt === B(0, 5 bits)
  val isRdZero = rd === B(0, 5 bits)

  val isJr = isSpecial && isRtZero && isRdZero && (func === B"001000")
  val isJalr = isSpecial && isRtZero && (func === B"001001")
  val isBltz = isRegImm && (rt === B"00000")
  val isBgez = isRegImm && (rt === B"00001")
  val isBltzal = isRegImm && (rt === B"10000")
  val isBgezal = isRegImm && (rt === B"10001")
  val isBeq = opcode === B"000100"
  val isBne = opcode === B"000101"
  val isBlez = (opcode === B"000110") && isRtZero
  val isBgtz = (opcode === B"000111") && isRtZero
  val isJ = opcode === B"000010"
  val isJal = opcode === B"000011"

  bpuEn := isJr || isJalr || isJ || isJal || isBltz || isBgez || isBltzal || isBgezal || isBeq || isBne || isBlez || isBgtz
  import BpOp._
  bpuOp := MuxOH(
    Vec(
      isJr || isJalr,
      isJ || isJal,
      isBltz || isBgez || isBltzal || isBgezal || isBeq || isBne || isBlez || isBgtz
    ),
    Vec(
      R(),
      J(),
      B()
    )
  )

  offset := MuxOH(
    Vec(
      isBltz | isBgez | isBltzal | isBgezal | isBeq | isBne | isBlez | isBgtz,
      isJ | isJal,
      isJr | isJalr
    ),
    Vec(
      U((13 downto 0) -> imm(15)) @@ imm.asUInt @@ U(0, 2 bits),
      pc(31 downto 28) @@ instIdx.asUInt @@ U(0, 2 bits),
      U(0, 32 bits)
    )
  )
}
