package j1cpu.cpu.blackbox

import spinal.core._

class xpm_memory_tdpram(depth: Int, width: Int, use4Data: Int) extends BlackBox {
  addGeneric("ADDR_WIDTH_A", log2Up(depth))
  addGeneric("ADDR_WIDTH_B", log2Up(depth))
  addGeneric("BYTE_WRITE_WIDTH_A", (if (use4Data == 0) width else 8))
  addGeneric("BYTE_WRITE_WIDTH_B", (if (use4Data == 0) width else 8))
  addGeneric("CLOCKING_MODE", "common_clock")
  addGeneric("MEMORY_PRIMITIVE", "block")
  addGeneric("MEMORY_SIZE", depth * width)
  addGeneric("READ_DATA_WIDTH_A", width)
  addGeneric("READ_DATA_WIDTH_B", width)
  addGeneric("READ_LATENCY_A", 1)
  addGeneric("READ_LATENCY_B", 1)
  addGeneric("WRITE_DATA_WIDTH_A", width)
  addGeneric("WRITE_DATA_WIDTH_B", width)
  addGeneric("WRITE_MODE_A", "read_first")
  addGeneric("WRITE_MODE_B", "read_first")

  val io = new Bundle {
    val clka = in Bool()

    // a for write
    val ena = in Bool()
    val wea = in Bits ((if (use4Data == 0) 1 else width / 8) bits)
    val addra = in UInt (log2Up(depth) bits)
    val dina = in UInt (width bits)
    val douta = out UInt (width bits)

    // b for read
    val enb = in Bool()
    val web = in Bits ((if (use4Data == 0) 1 else width / 8) bits)
    val addrb = in UInt (log2Up(depth) bits)
    val dinb = in UInt (width bits)
    val doutb = out UInt (width bits)
  }

  noIoPrefix()

  mapClockDomain(clock = io.clka)
}
