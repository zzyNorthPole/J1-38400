package j1cpu.cpu.plugins.ID

import j1cpu.cpu.J1cpu
import j1cpu.cpu.signals.{AluOp, CacheOp, Cp0Reg, JuOp, MduOp, MemOp, TlbOp, TuOp, WbSrc}
import j1cpu.cpu.vexriscv.{Pipeline, Plugin}
import spinal.core._
import spinal.lib.MuxOH

class Decoder extends Component {
  val io = new Bundle {
    val pc = in UInt (32 bits)
    val inst = in Bits (32 bits)
    // ID regFill
    val rPorts = new Bundle {
      val en = out Vec(Bool(), 2)
      val addr = out Vec(UInt(5 bits), 2)
    }
    val immediate = out UInt (32 bits)
    // EX alu
    val aluEn = out Bool()
    val aluOp = out(AluOp())
    // EX mdu
    val mduEn = out Bool()
    val mduOp = out(MduOp())
    // EX ju
    val juEn = out Bool()
    val juOp = out(JuOp())
    // MEM load/store/cache
    val memEn = out Bool()
    val memW = out Bool()
    val memOp = out(MemOp())
    val tlbOpEn = out Bool()
    val tlbOp = out(TlbOp())
    val iCacheOpEn = out Bool()
    val dCacheOpEn = out Bool()
    val cacheOp = out(CacheOp())
    // MEM cp0
    val cp0En = out Bool()
    val cp0W = out Bool()
    val cp0Reg = out (Cp0Reg())
    val cp0Select = out UInt (3 bits)
    val wbEn = out Bool()
    val wbSrc = out(WbSrc())
    val wbReg = out UInt (5 bits)
    // EX tu
    val tuEn = out Bool()
    val tuOp = out(TuOp())

    // exception
    val eret = out Bool()
    val syscall = out Bool()
    val break = out Bool()
    val reserveInst = out Bool()
    val copUnusable = out Bool()
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

  // r-type
  val isSpecial = opcode === B(0, 6 bits)
  val isCop0 = opcode === B"010000"
  val isSpecial2 = opcode === B"011100"
  val isRegImm = opcode === B"000001"
  val isSaZero = sa === B(0, 5 bits)
  val isRsZero = rs === B(0, 5 bits)
  val isRtZero = rt === B(0, 5 bits)
  val isRdZero = rd === B(0, 5 bits)
  val isSll = isSpecial && isRsZero && (func === B"000000")
  val isSrl = isSpecial && isRsZero && (func === B"000010")
  val isSra = isSpecial && isRsZero && (func === B"000011")
  val isSllv = isSpecial && isSaZero && (func === B"000100")
  val isSrlv = isSpecial && isSaZero && (func === B"000110")
  val isSrav = isSpecial && isSaZero && (func === B"000111")
  val isJr = isSpecial && isRtZero && isRdZero && (func === B"001000")
  val isJalr = isSpecial && isRtZero && (func === B"001001")
  val isMovz = isSpecial && isSaZero && (func === B"001010")
  val isMovn = isSpecial && isSaZero && (func === B"001011")
  val isSyscall = isSpecial && (func === B"001100")
  val isBreak = isSpecial && (func === B"001101")
  val isSync = isSpecial && (func === B"001111") // nop
  val isMfhi = isSpecial && isRsZero && isRtZero && isSaZero && (func === B"010000")
  val isMthi = isSpecial && isRtZero && isRdZero && isSaZero && (func === B"010001")
  val isMflo = isSpecial && isRsZero && isRtZero && isSaZero && (func === B"010010")
  val isMtlo = isSpecial && isRtZero && isRdZero && isSaZero && (func === B"010011")
  val isMult = isSpecial && isRdZero && isSaZero && (func === B"011000")
  val isMultu = isSpecial && isRdZero && isSaZero && (func === B"011001")
  val isDiv = isSpecial && isRdZero && isSaZero && (func === B"011010")
  val isDivu = isSpecial && isRdZero && isSaZero && (func === B"011011")
  val isAdd = isSpecial && isSaZero && (func === B"100000")
  val isAddu = isSpecial && isSaZero && (func === B"100001")
  val isSub = isSpecial && isSaZero && (func === B"100010")
  val isSubu = isSpecial && isSaZero && (func === B"100011")
  val isAnd = isSpecial && isSaZero && (func === B"100100")
  val isOr = isSpecial && isSaZero && (func === B"100101")
  val isXor = isSpecial && isSaZero && (func === B"100110")
  val isNor = isSpecial && isSaZero && (func === B"100111")
  val isSlt = isSpecial && isSaZero && (func === B"101010")
  val isSltu = isSpecial && isSaZero && (func === B"101011")
  val isTge = isSpecial && (func === B"110000")
  val isTgeu = isSpecial && (func === B"110001")
  val isTlt = isSpecial && (func === B"110010")
  val isTltu = isSpecial && (func === B"110011")
  val isTeq = isSpecial && (func === B"110100")
  val isTne = isSpecial && (func === B"110110")
  val isMfc0 = isCop0 && (rs === B"00000") && isSaZero
  val isMtc0 = isCop0 && (rs === B"00100") && isSaZero
  val isTlbr = isCop0 && (rs === B"10000") && isRtZero && isRdZero && isSaZero && (func === B"000001")
  val isTlbwi = isCop0 && (rs === B"10000") && isRtZero && isRdZero && isSaZero && (func === B"000010")
  val isTlbwr = isCop0 && (rs === B"10000") && isRtZero && isRdZero && isSaZero && (func === B"000110")
  val isTlbp = isCop0 && (rs === B"10000") && isRtZero && isRdZero && isSaZero && (func === B"001000")
  val isEret = isCop0 && (rs === B"10000") && isRtZero && isRdZero && isSaZero && (func === B"011000")
  val isWait = isCop0 && (func === B"100000") // nop
  val isMul = isSpecial2 && isSaZero && (func === B"000010")
  val isClz = isSpecial2 && isSaZero && (func === B"100000")
  val isClo = isSpecial2 && isSaZero && (func === B"100001")
  val isMadd = isSpecial2 && isRdZero && isSaZero && (func === B"000000")
  val isMaddu = isSpecial2 && isRdZero && isSaZero && (func === B"000001")
  val isMsub = isSpecial2 && isRdZero && isSaZero && (func === B"000100")
  val isMsubu = isSpecial2 && isRdZero && isSaZero && (func === B"000101")

  // i-type
  val isBltz = isRegImm && (rt === B"00000")
  val isBgez = isRegImm && (rt === B"00001")
  val isTgei = isRegImm && (rt === B"01000")
  val isTgeiu = isRegImm && (rt === B"01001")
  val isTlti = isRegImm && (rt === B"01010")
  val isTltiu = isRegImm && (rt === B"01011")
  val isTeqi = isRegImm && (rt === B"01100")
  val isTnei = isRegImm && (rt === B"01110")
  val isBltzal = isRegImm && (rt === B"10000")
  val isBgezal = isRegImm && (rt === B"10001")
  val isBeq = opcode === B"000100"
  val isBne = opcode === B"000101"
  val isBlez = (opcode === B"000110") && isRtZero
  val isBgtz = (opcode === B"000111") && isRtZero
  val isAddi = opcode === B"001000"
  val isAddiu = opcode === B"001001"
  val isSlti = opcode === B"001010"
  val isSltiu = opcode === B"001011"
  val isAndi = opcode === B"001100"
  val isOri = opcode === B"001101"
  val isXori = opcode === B"001110"
  val isLui = (opcode === B"001111") && isRsZero
  val isLb = opcode === B"100000"
  val isLh = opcode === B"100001"
  val isLwl = opcode === B"100010"
  val isLw = opcode === B"100011"
  val isLbu = opcode === B"100100"
  val isLhu = opcode === B"100101"
  val isLwr = opcode === B"100110"
  val isSb = opcode === B"101000"
  val isSh = opcode === B"101001"
  val isSwl = opcode === B"101010"
  val isSw = opcode === B"101011"
  val isSwr = opcode === B"101110"
  val isCache = opcode === B"101111"
  val isPref = opcode === B"110011" // nop

  // j-type
  val isJ = opcode === B"000010"
  val isJal = opcode === B"000011"

  rPorts.en(0) := (
    isAdd || isAddu || isSub || isSubu || isAnd || isOr || isXor || isNor || isSlt || isSltu ||
      isSllv || isSrlv || isSrav ||
      isAddi || isAddiu || isSlti || isSltiu || isAndi || isOri || isXori ||
      isLb || isLh || isLwl || isLw || isLbu || isLhu || isLwr ||
      isSb || isSh || isSwl || isSw || isSwr ||
      isCache ||
      isClo || isClz ||
      isMult || isMultu || isMul || isDiv || isDivu ||
      isMadd || isMaddu || isMsub || isMsubu ||
      isMthi || isMtlo ||
      isMovn || isMovz ||
      isBltz || isBgez || isBltzal || isBgezal || isBeq || isBne || isBlez || isBgtz ||
      isJr || isJalr ||
      isTeq || isTne || isTge || isTgeu || isTlt || isTltu ||
      isTeqi || isTnei || isTgei || isTgeiu || isTlti || isTltiu ||
      isMovn || isMovz
  )
  rPorts.en(1) := (
    isAdd || isAddu || isSub || isSubu || isAnd || isOr || isXor || isNor || isSlt || isSltu ||
      isSll || isSrl || isSra || isSllv || isSrlv || isSrav ||
      isLwl || isLwr ||
      isSb || isSh || isSwl || isSw || isSwr ||
      isMult || isMultu || isMul || isDiv || isDivu ||
      isMadd || isMaddu || isMsub || isMsubu ||
      isMovn || isMovz ||
      isBeq || isBne ||
      isMtc0 ||
      isTeq || isTne || isTge || isTgeu || isTlt || isTltu ||
      isMovn || isMovz
  )
  rPorts.addr(0) := rs.asUInt
  rPorts.addr(1) := rt.asUInt

  aluEn := (
    isAdd || isAddi || isAddu || isAddiu ||
      isSub || isSubu ||
      isAnd || isAndi ||
      isOr || isOri ||
      isXor || isXori ||
      isNor ||
      isSlt || isSlti || isSltu || isSltiu ||
      isLui ||
      isSll || isSllv ||
      isSrl || isSrlv ||
      isSra || isSrav ||
      isMovn || isMovz ||
      isClo || isClz
  )
  import AluOp._
  aluOp := MuxOH(
    Vec(
      isAdd | isAddi,
      isAddu | isAddiu,
      isSub,
      isSubu,
      isAnd | isAndi,
      isOr | isOri,
      isXor | isXori,
      isNor,
      isSlt | isSlti,
      isSltu | isSltiu,
      isLui,
      isSll | isSllv,
      isSrl | isSrlv,
      isSra | isSrav,
      isMovn,
      isMovz,
      isClo,
      isClz
    ),
    Vec(
      ADD(),
      ADDU(),
      SUB(),
      SUBU(),
      AND(),
      OR(),
      XOR(),
      NOR(),
      SLT(),
      SLTU(),
      LUI(),
      SLL(),
      SRL(),
      SRA(),
      MOVN(),
      MOVZ(),
      CLO(),
      CLZ()
    )
  )

  mduEn := isMul | isMult | isMultu | isDiv | isDivu | isMfhi | isMthi | isMflo | isMtlo
  import MduOp._
  mduOp := MuxOH(
    Vec(
      isMul,
      isMult,
      isMultu,
      isDiv,
      isDivu,
      isMfhi,
      isMthi,
      isMflo,
      isMtlo
    ),
    Vec(
      MUL(),
      MULT(),
      MULTU(),
      DIV(),
      DIVU(),
      MFHI(),
      MTHI(),
      MFLO(),
      MTLO()
    )
  )

  juEn := isJal | isJalr | isJ | isJr | isBltz | isBgez | isBltzal | isBgezal | isBeq | isBne | isBlez | isBgtz
  import JuOp._
  juOp := MuxOH(
    Vec(
      isBltz,
      isBgez,
      isBltzal,
      isBgezal,
      isBeq,
      isBne,
      isBlez,
      isBgtz,
      isJr,
      isJalr,
      isJ,
      isJal
    ),
    Vec(
      BLTZ(),
      BGEZ(),
      BLTZAL(),
      BGEZAL(),
      BEQ(),
      BNE(),
      BLEZ(),
      BGTZ(),
      JR(),
      JALR(),
      J(),
      JAL()
    )
  )

  memEn := isLb || isLh || isLwl || isLw || isLbu || isLhu || isLwr ||
    isSb || isSh || isSwl || isSw || isSwr || isCache

  memW := isSb | isSh | isSwl | isSw | isSwr

  import MemOp._
  memOp := MuxOH(
    Vec(
      isLb | isSb,
      isLh | isSh,
      isLwl | isSwl,
      isLw | isSw,
      isLbu,
      isLhu,
      isLwr | isSwr
    ),
    Vec(
      B(),
      H(),
      WL(),
      W(),
      BU(),
      HU(),
      WR()
    )
  )

  tlbOpEn := isTlbp | isTlbr | isTlbwi | isTlbwr
  import TlbOp._
  tlbOp := MuxOH(
    Vec(
      isTlbp,
      isTlbr,
      isTlbwi,
      isTlbwr
    ),
    Vec(
      TLBP(),
      TLBR(),
      TLBWI(),
      TLBWR()
    )
  )

  iCacheOpEn := isCache && (rt(1 downto 0) === B"00")
  dCacheOpEn := isCache && (rt(1 downto 0) === B"01")
  import CacheOp._
  cacheOp := !isCache ? cacheOpMiss | (
    iCacheOpEn ?
      rt(4 downto 2).mux(
        B"000" -> indexInvalidateWriteBack(),
        B"100" -> hitInvalidateNotWriteBack(),
        default -> cacheOpMiss()
      ) |
      rt(4 downto 2).mux(
        B"000" -> indexInvalidateWriteBack(),
        B"100" -> hitInvalidateNotWriteBack(),
        B"101" -> hitInvalidateWriteBack(),
        default -> cacheOpMiss()
      )
  )

  cp0En := isMfc0 | isMtc0
  cp0W := isMtc0
  cp0Reg := (rd.asUInt).mux(
    U(0, 5 bits) -> Cp0Reg.Index(),
    U(1, 5 bits) -> Cp0Reg.Random(),
    U(2, 5 bits) -> Cp0Reg.EntryLo0(),
    U(3, 5 bits) -> Cp0Reg.EntryLo1(),
    U(4, 5 bits) -> Cp0Reg.Context(),
    U(5, 5 bits) -> Cp0Reg.PageMask(),
    U(6, 5 bits) -> Cp0Reg.Wired(),
    U(8, 5 bits) -> Cp0Reg.BadVAddr(),
    U(9, 5 bits) -> Cp0Reg.Count(),
    U(10, 5 bits) -> Cp0Reg.EntryHi(),
    U(11, 5 bits) -> Cp0Reg.Compare(),
    U(12, 5 bits) -> Cp0Reg.Status(),
    U(13, 5 bits) -> Cp0Reg.Cause(),
    U(14, 5 bits) -> Cp0Reg.EPC(),
    U(15, 5 bits) -> Cp0Reg.PRIdEBase(),
    U(16, 5 bits) -> Cp0Reg.Config(),
    U(30, 5 bits) -> Cp0Reg.ErrorEPC(),
    default -> Cp0Reg.Index()
  )
  cp0Select := func(2 downto 0).asUInt

  tuEn := isTeq | isTeqi | isTne | isTnei | isTge | isTgei | isTgeu | isTgeiu | isTlt | isTlti | isTltu | isTltiu
  tuOp := MuxOH(
    Vec(
      isTeq | isTeqi,
      isTne | isTnei,
      isTge | isTgei,
      isTgeu | isTgeiu,
      isTlt | isTlti,
      isTltu | isTltiu
    ),
    Vec(
      TuOp.TEQ(),
      TuOp.TNE(),
      TuOp.TGE(),
      TuOp.TGEU(),
      TuOp.TLT(),
      TuOp.TLTU()
    )
  )

  immediate := MuxOH(
    Vec(
      isAddi | isAddiu | isSlti | isSltiu |
        isLb | isLh | isLwl | isLw | isLbu | isLhu | isLwr | isSb | isSh | isSwl | isSw | isSwr |
        isTeqi | isTnei | isTgei | isTgeiu | isTlti | isTltiu |
        isCache | isPref,
      isAndi | isOri | isXori | isLui,
      isSll | isSrl | isSra,
      isBltz | isBgez | isBltzal | isBgezal | isBeq | isBne | isBlez | isBgtz,
      isJ | isJal
    ),
    Vec(
      U((15 downto 0) -> imm(15)) @@ imm.asUInt,
      U(0, 16 bits) @@ imm.asUInt,
      U(0, 27 bits) @@ sa.asUInt,
      U((13 downto 0) -> imm(15)) @@ imm.asUInt @@ U(0, 2 bits),
      pc(31 downto 28) @@ instIdx.asUInt @@ U(0, 2 bits)
    )
  )

  wbEn := (
    isAdd | isAddu | isSub | isSubu | isAddi | isAddiu |
      isAnd | isOr | isXor | isNor | isAndi | isOri | isXori |
      isSll | isSrl | isSra | isSllv | isSrlv | isSrav |
      isSlt | isSltu | isSlti | isSltiu | isLui |
      isMul | isMfhi | isMthi | isMflo | isMflo |
      isJalr | isJal | isBltzal | isBgezal |
      isLb | isLh | isLwl | isLw | isLbu | isLhu | isLwr |
      isMfc0 |
      isMovn | isMovz |
      isClo | isClz
  )

  import WbSrc._
  wbSrc := MuxOH(
    Vec(
      isAdd | isAddu | isSub | isSubu | isAddi | isAddiu |
        isAnd | isOr | isXor | isNor | isAndi | isOri | isXori |
        isSll | isSrl | isSra | isSllv | isSrlv | isSrav |
        isSlt | isSltu | isSlti | isSltiu | isLui |
        isMovn | isMovz |
        isClo | isClz,
      isMul | isMfhi | isMflo,
      isJalr | isJal | isBltzal | isBgezal,
      isLb | isLh | isLwl | isLw | isLbu | isLhu | isLwr,
      isMfc0
    ),
    Vec(
      Alu(),
      Mdu(),
      Ju(),
      DCache(),
      Cp0()
    )
  )

  wbReg := MuxOH(
    Vec(
      isAdd | isAddu | isSub | isSubu |
        isAnd | isOr | isXor | isNor |
        isSll | isSrl | isSra | isSllv | isSrlv | isSrav |
        isSlt | isSltu |
        isClo | isClz |
        isMul |
        isMfhi | isMflo |
        isMovn | isMovz |
        isJalr | isJal |
        isMovn | isMovz |
        isClo | isClz,
      isAddi | isAddiu |
        isAndi | isOri | isXori |
        isSlti | isSltiu | isLui |
        isLb | isLh | isLwl | isLw | isLbu | isLhu | isLwr |
        isMfc0,
      isBltzal | isBgezal | isJal | isJalr
    ),
    Vec(
      rd,
      rt,
      B"11111"
    )
  ).asUInt

  eret := isEret
  syscall := isSyscall
  break := isBreak
  reserveInst := ~(
    isAdd | isAddu | isSub | isSubu | isAnd | isOr | isXor | isNor |
      isSll | isSrl | isSra | isSllv | isSrlv | isSrav |
      isSlt | isSltu |
      isAddi | isAddiu | isSlti | isSltiu | isAndi | isOri | isXori | isLui |
      isLb | isLh | isLwl | isLw | isLbu | isLhu | isLwr |
      isSb | isSh | isSwl | isSw | isSwr |
      isMult | isMultu | isDiv | isDivu | isMul |
      isMfhi | isMthi |
      isMflo | isMtlo |
      isBltz | isBgez | isBltzal | isBgezal | isBeq | isBne | isBlez | isBgtz |
      isJr | isJalr | isJ | isJal |
      isMfc0 | isMtc0 |
      isSyscall | isBreak | isSync | isWait |
      isCache | isPref |
      isTlbr | isTlbwi | isTlbwr | isTlbp |
      isEret |
      isTeq | isTne | isTge | isTgeu | isTlt | isTltu |
      isTeqi | isTnei | isTgei | isTgeiu | isTlti | isTltiu |
      isMovn | isMovz |
      isClo | isClz
  )
  copUnusable := False
}
