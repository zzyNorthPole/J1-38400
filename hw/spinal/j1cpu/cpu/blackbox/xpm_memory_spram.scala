package j1cpu.cpu.blackbox

import spinal.core._
import spinal.lib._

class xpm_memory_spram(depth: Int, width: Int, use4Data: Int) extends BlackBox {
  addGeneric("ADDR_WIDTH_A", log2Up(depth))
  // byte write width: enable byte-wide writes on port A, specify the byte width in bits
  // for example set it as 8, means a byte is 8 bits width
  addGeneric("BYTE_WRITE_WIDTH_A", (if (use4Data == 0) width else 8))
  addGeneric("MEMORY_PRIMITIVE", "block")
  addGeneric("MEMORY_SIZE", depth * width)
  addGeneric("READ_DATA_WIDTH_A", width)
  addGeneric("READ_LATENCY_A", 1)
  addGeneric("WRITE_DATA_WIDTH_A", width)
  addGeneric("WRITE_MODE_A", "read_first") // TODO maybe change to write_first

  val io = new Bundle {
    val clka = in Bool()
    val ena = in Bool()
    val wea = in UInt((if (use4Data == 0) 1 else (width / 8)) bits)
    val addra = in UInt(log2Up(depth) bits)
    val dina = in UInt(width bits)
    val douta = out UInt(width bits)
  }

  noIoPrefix()

  // mapClockDomain(clock = io.clka)
}
