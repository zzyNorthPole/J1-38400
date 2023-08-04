package j1cpu.cpu.plugins.IF

import j1cpu.cpu.BpuConfig
import j1cpu.cpu.utils.{Bram}
import spinal.core._

class Bpu(bpuConfig: BpuConfig, sim: Int) extends Component {
  val io = new Bundle {
    val predict = new Bundle {
      val isStalled = in Bool() // if1
      val exception = in Bool() // if2

      val en = in Bool() // if1
      val pc = in UInt (32 bits) // if1

      val hit = out Bool() // if2
      val isBranchPredict = out Bool() // if2
      val branchPredictAddr = out UInt (32 bits) // if2
      val branchHistoryRegister = out UInt(2 bits) // if2
    }

    val calibrate = new Bundle {
      val en = in Bool() // ex
      val pc = in UInt(32 bits) // ex
      val branchTarget = in UInt(32 bits) // ex
      val branchHistoryRegister = in UInt(2 bits) // ex
    }
  }
  noIoPrefix()

  val tagRams = new Bram(bpuConfig.lines, bpuConfig.tagWidth, 0, sim)

  val validRams = new Bram(bpuConfig.lines, 1, 0, 2)

  val targetRams = new Bram(bpuConfig.lines, 32, 0, sim)

  val bhrRams = new Bram(bpuConfig.lines, 2, 0, sim)

  val if1 = new Area {
    val pc = UInt(32 bits)
    pc := io.predict.pc

    val index = UInt(bpuConfig.indexWidth bits)
    index := pc(bpuConfig.indexWidth + 1 downto 2)

    val rPortsInit = new Area {
      // branch target buffer
      val btb = new Area {
        tagRams.io.enb := io.predict.en
        tagRams.io.web := B(0, 1 bits)
        tagRams.io.addrb := index
        tagRams.io.dinb := U(0, bpuConfig.tagWidth bits)

        validRams.io.enb := io.predict.en
        validRams.io.web := B(0, 1 bits)
        validRams.io.addrb := index
        validRams.io.dinb := U(0, 1 bits)

        targetRams.io.enb := io.predict.en
        targetRams.io.web := B(0, 1 bits)
        targetRams.io.addrb := index
        targetRams.io.dinb := U(0, 32 bits)
      }

      // branch history register = bhr
      // branch history table = bht
      val bht = new Area {
        bhrRams.io.enb := True
        bhrRams.io.web := B(0, 1 bits)
        bhrRams.io.addrb := index
        bhrRams.io.dinb := U(0, 2 bits)
      }
    }
  }

  val if2 = new Area {
    val pc = RegInit(U(0, 32 bits))

    when(!io.predict.isStalled) {
      pc := if1.pc
    }

    val correctTag = UInt(bpuConfig.tagWidth bits)
    correctTag := pc(31 downto (bpuConfig.indexWidth + 2))

    val tag = UInt(bpuConfig.tagWidth bits)
    tag := tagRams.io.doutb

    val valid = UInt(1 bits)
    valid := validRams.io.doutb

    val hit = Bool()
    hit := valid(0) && (tag === correctTag)

    val target = UInt(32 bits)
    target := targetRams.io.doutb

    val bhr = UInt(2 bits)
    bhr := bhrRams.io.doutb

    io.predict.hit := hit
    io.predict.isBranchPredict := hit && !io.predict.exception
    io.predict.branchPredictAddr := hit ? (bhr(1) ? target | (pc + 8)) | (pc + 8)
    io.predict.branchHistoryRegister := hit ? bhr | U"01"
  }

  val ex = new Area {
    val pc = UInt(32 bits)
    pc := io.calibrate.pc

    val tag = UInt(bpuConfig.tagWidth bits)
    tag := pc(31 downto (bpuConfig.indexWidth + 2))

    val index = UInt(bpuConfig.indexWidth bits)
    index := pc((bpuConfig.indexWidth + 1) downto 2)

    val wPortsInit = new Area {
      val targetCache = new Area {
        tagRams.io.ena := io.calibrate.en
        tagRams.io.wea := io.calibrate.en.asBits
        tagRams.io.addra := index
        tagRams.io.dina := tag

        validRams.io.ena := io.calibrate.en
        validRams.io.wea := io.calibrate.en.asBits
        validRams.io.addra := index
        validRams.io.dina := U(1, 1 bits)

        targetRams.io.ena := io.calibrate.en
        targetRams.io.wea := io.calibrate.en.asBits
        targetRams.io.addra := index
        targetRams.io.dina := io.calibrate.branchTarget
      }

      val bhr = new Area {
        bhrRams.io.ena := io.calibrate.en
        bhrRams.io.wea := io.calibrate.en.asBits
        bhrRams.io.addra := index
        bhrRams.io.dina := io.calibrate.branchHistoryRegister
      }
    }
  }
}
