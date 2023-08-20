package j1cpu.cpu.plugins.MEM

import j1cpu.cpu.WriteQueueConfig
import j1cpu.cpu.signals.MemOp
import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axi.{Axi4Config, Axi4WriteOnly}
import spinal.lib.fsm.{State, StateMachine}

class WriteQueueItem extends Bundle {
  val addr = UInt(32 bits)
  val size = UInt(3 bits)
  val strb = Bits(4 bits)
  val data = Bits(32 bits)
}
class WriteQueue(writeQueueConfig: WriteQueueConfig, axiConfig: Axi4Config) extends Component {
  val io = new Bundle {
    val empty = out Bool()
    val full = out Bool()

    val en = in Bool()
    val din = in(new WriteQueueItem)

    val udbus = master(Axi4WriteOnly(axiConfig)).setIdle()
  }
  noIoPrefix()

  import io._

  val front = RegInit(U(0, log2Up(writeQueueConfig.lines) bits))
  val end = RegInit(U(0, log2Up(writeQueueConfig.lines) bits))

  val writeQueue = Vec(Reg(new WriteQueueItem), writeQueueConfig.lines)

  val udbusInit = new Area {
    import io.udbus._

    aw.addr := writeQueue(front).addr
    aw.id := U(1, 4 bits);
    aw.len := U(0, 8 bits)
    aw.size := writeQueue(front).size
    aw.burst := B(1, 2 bits) // INCR
    // aw.lock := B(0, 2 bits)
    aw.cache := B(0, 4 bits)
    aw.prot := B(0, 3 bits)

    w.data := writeQueue(front).data
    w.strb := writeQueue(front).strb
    w.last := True
  }

  empty := front === end
  full := (end + 1) === front

  when(en) {
    end := end + 1
    writeQueue(end) := din
  }

  val dataUncachedWriteFSM = new StateMachine {

    import io.udbus._

    setEntry(stateBoot)
    disableAutoStart()

    val writeAw = new State()
    val writeW = new State()
    val writeB = new State()

    stateBoot.whenIsActive {
      when(!empty) {
        goto(writeAw)
      }
    }

    writeAw.whenIsActive {
      aw.valid := True
      when(aw.ready) {
        goto(writeW)
      }
    }

    writeW.whenIsActive {
      w.valid := True
      when(w.ready) {
        goto(writeB)
      }
    }

    writeB.whenIsActive {
      b.ready := True
      when(b.valid) {
        front := front + 1
        goto(stateBoot)
      }
    }
  }
}
