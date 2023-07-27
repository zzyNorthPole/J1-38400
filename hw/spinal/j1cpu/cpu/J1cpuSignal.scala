package j1cpu.cpu

import j1cpu.cpu.plugins.MMU.TLBRequest
import spinal.core._
import spinal.lib._
import j1cpu.cpu.vexriscv._
import j1cpu.cpu.signals._

class J1cpuSignal() {
  // IF1 to
  object PC extends Signal(UInt(32 bits))
  // IF2 to ID
  object INST extends Signal(Bits(32 bits))
  // ID to MEM
  object DELAY_SLOT extends Signal(Bool())
  // ID to MEM
  object DELAY_SLOT_NEXT_PC extends Signal(UInt(32 bits))
  // Alu related
  // ID to EX
  object ALU_EN extends Signal(Bool())
  // ID to EX
  object ALU_OP extends Signal(AluOp())
  // ID to EX
  object ALU_DIN1 extends Signal(UInt(32 bits))
  // ID to EX
  object ALU_DIN2 extends Signal(UInt(32 bits))
  // EX to WB
  object ALU_RESULT extends Signal(UInt(32 bits))
  // Mdu related
  // ID to EX
  object MDU_EN extends Signal(Bool())
  // ID to EX
  object MDU_OP extends Signal(MduOp())
  // ID to EX
  object MDU_DIN1 extends Signal(UInt(32 bits))
  // ID to EX
  object MDU_DIN2 extends Signal(UInt(32 bits))
  // EX to WB
  object MDU_RESULT extends Signal(UInt(32 bits))
  // Ju related
  // ID to EX
  object PREDICT_PC extends Signal(UInt(32 bits))
  // ID to EX
  object JU_EN extends Signal(Bool())
  // ID to EX
  object JU_OP extends Signal(JuOp())
  // ID to EX
  object JU_DIN1 extends Signal(SInt(32 bits))
  // ID to EX
  object JU_DIN2 extends Signal(SInt(32 bits))
  // ID to EX
  object JU_OFFSET extends Signal(UInt(32 bits))
  // EX to WB
  object JU_LINK extends Signal(UInt(32 bits))
  // Mem related
  // ID to EX
  object AGU_DIN1 extends Signal(UInt(32 bits))
  // ID to EX
  object AGU_DIN2 extends Signal(UInt(32 bits))
  // EX to MEM
  object MEM_ADDRESS extends Signal(UInt(32 bits))
  // ID to MEM
  object MEM_EN extends Signal(Bool())
  // ID to MEM
  object MEM_W extends Signal(Bool())
  // ID to MEM
  object MEM_OP extends Signal(MemOp())
  // ID to MEM
  object MEM_WE extends Signal(Bits(4 bits))
  // ID to MEM
  object MEM_DIN extends Signal(UInt(32 bits))
  // MEM to WB
  object MEM_RESULT extends Signal(UInt(32 bits))
  // TLB related
  // ID to MEM
  object TLB_OP_EN extends Signal(Bool())
  // ID to MEM
  object TLB_OP extends Signal(TlbOp())
  // CACHE related
  // ID to MEM
  object ICACHE_OP_EN extends Signal(Bool())
  // ID to MEM
  object DCACHE_OP_EN extends Signal(Bool())
  // ID to MEM
  object CACHE_OP extends Signal(CacheOp())
  // ID to MEM
  object CP0_EN extends Signal(Bool())
  // ID to MEM
  object CP0_W extends Signal(Bool())
  // ID to MEM
  object CP0_REG extends Signal(Cp0Reg())
  // ID to MEM
  object CP0_SELECT extends Signal(UInt(3 bits))
  // ID to MEM
  object CP0_DIN extends Signal(UInt(32 bits))
  // MEM to WB
  object CP0_RESULT extends Signal(UInt(32 bits))
  // 4 * 1 write back unit
  // ID to WB
  object WB_EN extends Signal(Bool())
  // ID to WB
  object WB_REG extends Signal(UInt(5 bits))
  // ID to WB
  object WB_SRC extends Signal(WbSrc())
  // ID to EX
  object TU_EN extends Signal(Bool())
  // ID to EX
  object TU_OP extends Signal(TuOp())
  // ID to EX
  object TU_DIN1 extends Signal(UInt(32 bits))
  // ID to EX
  object TU_DIN2 extends Signal(UInt(32 bits))
  // exceptions
  // IF to MEM
  object EX_EN extends Signal(Bool())
  // IF to MEM
  object EX_OP extends Signal(Exception())
  // IF to MEM
  object EX_TLB_HIT extends Signal(Bool())
  // IF to MEM
  object EX_BAD_ADDR extends Signal(UInt(32 bits))
  // IF to MEM
  object EX_BAD_TLB_REQUEST extends Signal(new TLBRequest)
  // ID to MEM
  object ERET extends Signal(Bool())
  // MEM to WB used for debug
  object DEBUG_COUNT extends Signal(UInt(32 bits))
  // MEM to WB used for debug
  object DEBUG_RANDOM extends Signal(UInt(32 bits))
  // MEM to WB used for debug
  object DEBUG_CAUSE extends Signal(UInt(32 bits))
}
