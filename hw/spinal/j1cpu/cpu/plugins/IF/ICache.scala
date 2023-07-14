package j1cpu.cpu.plugins.IF

import j1cpu.cpu.signals.CacheOp
import j1cpu.cpu.utils.{Bram, Dram, Lfsr}
import spinal.core._
import spinal.lib._
import j1cpu.cpu.{CacheConfig, J1cpu, J1cpuConfig, vexriscv}
import j1cpu.cpu.vexriscv.Plugin
import spinal.lib.bus.amba4.axi.{Axi4, Axi4Config, Axi4ReadOnly}
import spinal.lib.fsm.{State, StateMachine}

class ICache(cacheConfig: CacheConfig, axiConfig: Axi4Config, sim: Int) extends Component {
  val io = new Bundle {
    // cpu
    val flush = in Bool() // if1

    val en = in Bool() // if1
    val we = in Bits (4 bits) // if1
    val addr = in UInt (32 bits) // if1
    val dout = out UInt (32 bits) // if2

    val cacheOpEn = in Bool()
    val cacheOp = in(CacheOp())

    // tlb
    val correctTag = in UInt (cacheConfig.tagWidth bits) // if1

    // communicate signal for pipeline pass
    // ready recommendation:
    // if2 stage en == 1: hit = 1 ready = 1
    // if2 stage en == 1: hit = 0 wait until fsm back to idle to set ready = 1
    // if2 stage en == 0: ready = 1
    val ready = out Bool() // if2
    val valid = out Bool() // if2

    // axi
    val ibus = master(Axi4ReadOnly(axiConfig)).setIdle() // if2
  }
  noIoPrefix()

  io.ibus.flattenForeach {
    signal => {
      val tmpName = signal.getName()
      val tmpList = tmpName.split("_")
      signal.setName(tmpList(0) + "_" + tmpList(1) + "_" + tmpList(tmpList.size - 1))
    }
  }

  val tagRams = Seq.fill(cacheConfig.ways) {
    new Dram(cacheConfig.lines, cacheConfig.tagWidth, 0, sim)
  }

  val validRams = Seq.fill(cacheConfig.ways) {
    new Dram(cacheConfig.lines, 1, 0, sim)
  }

  val dataRams = Seq.fill(cacheConfig.words) {
    Seq.fill(cacheConfig.ways) {
      new Bram(cacheConfig.lines, 32, 1, sim)
    }
  }

  val if1 = new Area {
    val flush = Bool()

    val en = Bool()
    val we = Bits (4 bits)
    val addr = UInt (32 bits)

    val cacheOpEn = Bool()
    val cacheOp = CacheOp()

    val correctTag = UInt (cacheConfig.tagWidth bits)
    val index = UInt (cacheConfig.indexWidth bits)
    val offset = UInt (cacheConfig.offsetWidth bits)

    flush := io.flush

    en := io.en
    we := io.we
    addr := io.addr

    cacheOpEn := io.cacheOpEn
    cacheOp := io.cacheOp

    correctTag := io.correctTag
    index := addr((cacheConfig.indexWidth + cacheConfig.offsetWidth - 1) downto cacheConfig.offsetWidth)
    offset := addr((cacheConfig.offsetWidth - 1) downto 0)

    val rPortsInit = new Area {
      for (i <- 0 until cacheConfig.ways) {
        val curRam = tagRams(i)
        curRam.io.enb := en
        curRam.io.addrb := index
      }

      for (i <- 0 until cacheConfig.ways) {
        val curRam = validRams(i)
        curRam.io.enb := en
        curRam.io.addrb := index
      }

      for (i <- 0 until cacheConfig.words) {
        for (j <- 0 until cacheConfig.ways) {
          val curRam = dataRams(i)(j)
          curRam.io.enb := en
          curRam.io.addrb := index
        }
      }
    }

    // output signal gen
    val tags = Vec(UInt(cacheConfig.tagWidth bits), cacheConfig.ways)
    for (i <- 0 until cacheConfig.ways) {
      val curRam = tagRams(i)
      tags(i) := curRam.io.doutb
    }

    val valids = Vec(Bool(), cacheConfig.ways)
    for (i <- 0 until cacheConfig.ways) {
      val curRam = validRams(i)
      valids(i) := curRam.io.doutb.asBool
    }

    val hits = Vec(Bool(), cacheConfig.ways)
    for (i <- 0 until cacheConfig.ways) {
      hits(i) := valids(i) && (tags(i) === io.correctTag)
    }
    val hit = hits.orR
    val hitWay = MuxOH(
      hits,
      for (i <- 0 until cacheConfig.ways) yield {
        U(i, log2Up(cacheConfig.ways) bits)
      }
    )

    // actually mem2.dataLines, put it here in order to avoid recursive definition
    val dataLines = Vec(Vec(UInt(32 bits), cacheConfig.ways), cacheConfig.words)
    dataLines.setName("mem2").reflectNames()
    for (i <- 0 until cacheConfig.words) {
      for (j <- 0 until cacheConfig.ways) {
        val curRam = dataRams(i)(j)
        dataLines(i)(j) := curRam.io.doutb
      }
    }

    // random replace way generation, sequential
    val lfsrWidth = log2Up(cacheConfig.ways) + 2
    val lfsr = new Lfsr(lfsrWidth)
    lfsr.io.en := io.en & io.ready
    lfsr.io.seed := U((lfsrWidth - 1 downto 0) -> true)
    // actually if2.lfsrDout, put it here in order to avoid recursive definition
    val lfsrDout = UInt(lfsrWidth bits)
    lfsrDout := lfsr.io.dout
  }
  if1.setName("if1").reflectNames()

  val if2 = new Area {
    val en = RegInit(False)
    val we = RegInit(B(0, 4 bits))
    val addr = RegInit(U(0, 32 bits))

    val cacheOpEn = RegInit(False)
    val cacheOp = RegInit(CacheOp.indexInvalidateWriteBack)

    val tags = Vec(RegInit(U(0, cacheConfig.tagWidth bits)), cacheConfig.ways)
    val correctTag = RegInit(U(0, cacheConfig.tagWidth bits))

    val valids = Vec(RegInit(False), cacheConfig.ways)

    val index = RegInit(U(0, cacheConfig.indexWidth bits))
    val offset = RegInit(U(0, cacheConfig.offsetWidth bits))

    when(io.ready) {
      en := if1.en && !if1.flush
      we := if1.we
      addr := if1.addr

      cacheOpEn := if1.cacheOpEn
      cacheOp := if1.cacheOp

      tags := if1.tags
      correctTag := if1.correctTag

      valids := if1.valids

      index := if1.index
      offset := if1.offset
    }

    val hit = RegInit(False)
    val hitWay = RegInit(U(0, log2Up(cacheConfig.ways) bits))
    when(io.ready) {
      hit := if1.hit
      hitWay := if1.hitWay
    }

    val data = Vec(UInt(32 bits), cacheConfig.ways)
    for (i <- 0 until cacheConfig.ways) {
      data(i) := if1.dataLines(offset((cacheConfig.offsetWidth - 1) downto 2))(i)
    }

    // invalid way select
    // combination
    val invalids = Vec(Bool(), cacheConfig.ways)
    for (i <- 0 until cacheConfig.ways) {
      invalids(i) := ~valids(i)
    }
    val invalid = invalids.orR
    val invalidWay = MuxOH(
      OHMasking.first(invalids),
      for (i <- 0 until cacheConfig.ways) yield {
        U(i, log2Up(cacheConfig.ways) bits)
      }
    )

    // index invalidate way of cache operation
    val indexInvalidateWay = UInt(log2Up(cacheConfig.ways) bits)
    indexInvalidateWay := correctTag(log2Up(cacheConfig.ways) - 1 downto 0)
    // hit invalidate way of cache operation
    val hitInvalidateWay = UInt(log2Up(cacheConfig.ways) bits)
    hitInvalidateWay := hitWay

    val replaceWay = invalid.mux(
      False -> if1.lfsrDout(log2Up(cacheConfig.ways) - 1 downto 0),
      True -> invalidWay
    )
    val replaceWayReg = RegInit(U(0, log2Up(cacheConfig.ways) bits))
    val replaceWayTagReg = RegInit(U(0, cacheConfig.tagWidth bits))
    val replaceWayDataReg = Vec(RegInit(U(0, 32 bits)), cacheConfig.words)

    val ibusInit = new Area {
      import io.ibus._
      ar.addr := correctTag @@ index @@ U(0, cacheConfig.offsetWidth bits) // TODO maybe physical address error
      ar.id := U(1, 5 bits)
      ar.len := U(cacheConfig.words - 1, 8 bits)
      ar.size := U(2, 3 bits) // 4 bytes
      ar.burst := B(1, 2 bits) // INCR
      ar.lock := B(0, 1 bits)
      ar.cache := B(0, 4 bits)
      ar.prot := B(0, 3 bits)
    }
    val reLife = RegInit(False)
    val count = RegInit(U(0, log2Up(cacheConfig.words) bits))
    io.dout := U(0, 32 bits)
    val readNewValid = CombInit(False)
    val readNewReady = CombInit(False)
    val instCachedReadNewFSM = new StateMachine {
      import io.ibus._

      setEntry(stateBoot)
      disableAutoStart()

      val readNewAr = new State()
      val readNewR = new State()

      stateBoot.whenIsActive {
        when(readNewValid) {
          goto(readNewAr)
        }
      }

      readNewAr.whenIsActive {
        ar.valid := True
        when(ar.ready) {
          count := 0
          replaceWayTagReg := correctTag
          goto(readNewR)
        }
      }

      readNewR.whenIsActive {
        r.ready := True
        when(r.valid) {
          replaceWayDataReg(count) := r.data.asUInt
          count := count + 1
          when(r.last && r.fire) {
            readNewReady := True
            goto(stateBoot)
          }
        }
      }
    }
    val floodFillValid = CombInit(False)
    val instCachedFSM = new StateMachine {
      setEntry(stateBoot)
      disableAutoStart()

      val readNew = new State()
      val floodFill = new State()

      stateBoot.whenIsActive {
        // combination
        when(reLife) {
          io.valid := True
          io.ready := True
        }.otherwise {
          io.valid := en && hit
          io.ready := !en || io.valid
        }

        // sequential
        when(!io.ready) {
          // sequential
          reLife := True
          replaceWayReg := replaceWay
          // combination
          readNewValid := True
          goto(readNew)
        }.otherwise {
          reLife := False
          io.dout := reLife ? replaceWayDataReg(offset((cacheConfig.offsetWidth - 1) downto 2)) | data(hitWay)
        }
      }

      readNew.whenIsActive {
        io.valid := False
        io.ready := False
        when(readNewReady) {
          goto(floodFill)
        }
      }

      floodFill.whenIsActive {
        io.valid := False
        io.ready := False

        floodFillValid := True
        hit := True

        goto(stateBoot)
      }
    }

    val invalidateWay = (cacheOp === CacheOp.indexInvalidateWriteBack) ? indexInvalidateWay | hitInvalidateWay
    val wPortsInit = new Area {
      for (i <- 0 until cacheConfig.ways) {
        val curRam = tagRams(i)
        val curWay = U(i, log2Up(cacheConfig.ways) bits)
        val cachedFloodFill = floodFillValid && (replaceWayReg === curWay)
        curRam.io.ena := cachedFloodFill
        curRam.io.wea := B(1, 1 bits)
        curRam.io.addra := index
        curRam.io.dina := replaceWayTagReg
      }

      for (i <- 0 until cacheConfig.ways) {
        val curRam = validRams(i)
        val curWay = U(i, log2Up(cacheConfig.ways) bits)
        val cachedFloodFill = floodFillValid && (replaceWayReg === curWay)
        val cacheOpStateBootInvalidateValid = (cacheOp === CacheOp.indexInvalidateWriteBack) ? io.valid | (io.valid && hit)
        val cacheOpStateBootInvalidate = cacheOpEn && cacheOpStateBootInvalidateValid && (invalidateWay === curWay)
        curRam.io.ena := cachedFloodFill || cacheOpStateBootInvalidate
        curRam.io.wea := B(1, 1 bits)
        curRam.io.addra := index
        curRam.io.dina := cachedFloodFill ? U(1, 1 bits) | U(0, 1 bits)
      }

      for (i <- 0 until cacheConfig.words) {
        for (j <- 0 until cacheConfig.ways) {
          val curRam = dataRams(i)(j)
          val curWay = U(j, log2Up(cacheConfig.ways) bits)
          val cachedFloodFill = floodFillValid && (replaceWayReg === curWay)
          curRam.io.ena := cachedFloodFill
          curRam.io.wea := B((3 downto 0) -> true)
          curRam.io.addra := index
          curRam.io.dina := replaceWayDataReg(i)
        }
      }
    }
  }
  if2.setName("if2").reflectNames()

  // TODO: maybe i cache inst need to flush all instrument after it in ISA
}

object ICacheGen {
  def main(args: Array[String]): Unit = {
    val spinalConfig = SpinalConfig(
      targetDirectory = "hw/gen",
      defaultConfigForClockDomains = new J1cpuConfig().clockConfig
    )

    spinalConfig.generateVerilog(new ICache(CacheConfig(ways = 2, lines = 256, blockSize = 32), J1cpuConfig().axiConfig, 0))
  }
}