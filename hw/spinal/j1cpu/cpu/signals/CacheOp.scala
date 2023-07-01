package j1cpu.cpu.signals

import spinal.core.SpinalEnum

object CacheOp extends SpinalEnum{
  val indexInvalidateWriteBack = newElement()
  val hitInvalidateNotWriteBack = newElement()
  val hitInvalidateWriteBack = newElement()
}
