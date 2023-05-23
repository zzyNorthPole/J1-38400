package j1cpu.cpu.vexriscv

import spinal.core._

// use a Signal to record a signal in pipeline
// a signal insert from a certain plugin, input into pipeline and output on posedge clk
class Signal[T <: Data](dataType: => T) extends HardType[T](dataType) with Nameable {
  setWeakName(this.getClass.getSimpleName.replace("$", ""))
}