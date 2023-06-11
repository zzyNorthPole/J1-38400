package j1cpu.cpu.blackbox

import spinal.core._
import spinal.lib._

class xpm_memory_dpdistram(depth: Int, width: Int, use4Data: Int) extends BlackBox {
  addGeneric("ADDR_WIDTH_A", log2Up(depth))
  addGeneric("ADDR_WIDTH_B", log2Up(depth))
  // byte write width: enable byte-wide writes on port A, specify the byte width in bits
  // for example set it as 8, means a byte is 8 bits width
  addGeneric("BYTE_WRITE_WIDTH_A", (if (use4Data == 0) width else 8))
  addGeneric("CLOCKING_MODE", "common_clock") // set both port A and B with clka
  addGeneric("MEMORY_SIZE", depth * width)
  addGeneric("READ_DATA_WIDTH_A", width)
  addGeneric("READ_DATA_WIDTH_B", width)
  addGeneric("READ_LATENCY_A", 0)
  addGeneric("READ_LATENCY_B", 0)
  addGeneric("WRITE_DATA_WIDTH_A", width)

  val io = new Bundle {
    val clka = in Bool()

    // a for write and read
    val ena = in Bool()
    val wea = in UInt ((if (use4Data == 0) 1 else (width / 8)) bits)
    val addra = in UInt (log2Up(depth) bits)
    val dina = in UInt (width bits)
    val douta = out UInt (width bits)

    // b for read
    val enb = in Bool()
    val addrb = in UInt (log2Up(depth) bits)
    val doutb = out UInt (width bits)
  }

  noIoPrefix()

  // mapClockDomain(clock = io.clka)
}
