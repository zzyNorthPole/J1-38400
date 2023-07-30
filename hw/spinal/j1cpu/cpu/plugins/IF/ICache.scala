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
    val fetch = new Bundle {
      // cpu
      val flush = in Bool() // if1
      val exception = in Bool() // if1

      val en = in Bool() // if1
      val we = in Bits (4 bits) // if1
      val addr = in UInt (32 bits) // if1
      val dout = out UInt (32 bits) // if2

      // tlb
      val cached = in Bool() // mem1 used for uncached
      val correctTag = in UInt (cacheConfig.tagWidth bits) // if1

      // communicate signal for pipeline pass
      // isStalled recommendation:
      // if2 stage en == 1: hit = 1 isStalled = 0
      // if2 stage en == 1: hit = 0 wait until fsm back to idle to set isStalled = 0
      // if2 stage en == 0: isStalled = 0
      val isStalled = out Bool() // if2
      val ready = out Bool() // if2
    }

    // axi
    val ibus = master(Axi4ReadOnly(axiConfig)).setIdle() // if2

    val cacheOp = new Bundle {
      val flush = in Bool() // mem1
      val exception = in Bool() // mem1
      val isStalled = in Bool() // mem2

      val en = in Bool() // mem1
      val cacheOp = in(CacheOp()) // mem1
      val addr = in UInt(32 bits) // mem1
      val correctTag = in UInt(cacheConfig.tagWidth bits) // mem1
      val ready = in Bool() // mem2
    }
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
    new Bram(cacheConfig.lines, cacheConfig.tagWidth, 0, sim)
  }

  val validRams = Seq.fill(cacheConfig.ways) {
    new Bram(cacheConfig.lines, 1, 0, 2)
  }

  val dataRams = Seq.fill(cacheConfig.words) {
    Seq.fill(cacheConfig.ways) {
      new Bram(cacheConfig.lines, 32, 1, sim)
    }
  }

  val mem1 = new Area {
    val cacheOp = new Bundle {
      val flush = Bool() // mem1
      val exception = Bool() // mem1
      val en = Bool()
      val cacheOp = CacheOp()
      val addr = UInt(32 bits)
      val correctTag = UInt(cacheConfig.tagWidth bits)
      val index = UInt(cacheConfig.indexWidth bits)
      val offset = UInt(cacheConfig.offsetWidth bits)
    }

    cacheOp.flush := io.cacheOp.flush
    cacheOp.exception := io.cacheOp.exception
    cacheOp.en := io.cacheOp.en
    cacheOp.cacheOp := io.cacheOp.cacheOp
    cacheOp.addr := io.cacheOp.addr
    cacheOp.correctTag := io.cacheOp.correctTag
    cacheOp.index := cacheOp.addr((cacheConfig.indexWidth + cacheConfig.offsetWidth - 1) downto cacheConfig.offsetWidth)
    cacheOp.offset := cacheOp.addr((cacheConfig.offsetWidth - 1) downto 0)
  }
  mem1.setName("mem1").reflectNames()

  val mem2 = new Area {
    val cacheOp = new Bundle {
      val en = RegInit(False)
      val cacheOp = RegInit(CacheOp.indexInvalidateWriteBack)
      val addr = RegInit(U(0, 32 bits))

      val correctTag = RegInit(U(0, cacheConfig.tagWidth bits))

      val index = RegInit(U(0, cacheConfig.indexWidth bits))
      val offset = RegInit(U(0, cacheConfig.offsetWidth bits))
    }

    when(io.cacheOp.isStalled) {
      cacheOp.en := mem1.cacheOp.en & !mem1.cacheOp.flush & !mem1.cacheOp.exception
      cacheOp.cacheOp := mem1.cacheOp.cacheOp
      cacheOp.addr := mem1.cacheOp.addr

      cacheOp.correctTag := mem1.cacheOp.correctTag

      cacheOp.index := mem1.cacheOp.index
      cacheOp.offset := mem1.cacheOp.offset
    }

    val tags = Vec(UInt(cacheConfig.tagWidth bits), cacheConfig.ways)
    for (i <- 0 until cacheConfig.ways) {
      val curRams = tagRams(i)
      tags(i) := curRams.io.douta
    }

    val valids = Vec(Bool(), cacheConfig.ways)
    for (i <- 0 until cacheConfig.ways) {
      val curRams = validRams(i)
      valids(i) := curRams.io.douta.asBool
    }

    val hits = Vec(Bool(), cacheConfig.ways)
    for (i <- 0 until cacheConfig.ways) {
      hits(i) := valids(i) && (tags(i) === cacheOp.correctTag)
    }
    val hit = hits.orR
    val hitWay = MuxOH(
      hits,
      for (i <- 0 until cacheConfig.ways) yield {
        U(i, log2Up(cacheConfig.ways) bits)
      }
    )

    // index invalidate way of cache operation
    val indexInvalidateWay = UInt(log2Up(cacheConfig.ways) bits)
    indexInvalidateWay := cacheOp.correctTag(log2Up(cacheConfig.ways) - 1 downto 0)
    // hit invalidate way of cache operation
    val hitInvalidateWay = UInt(log2Up(cacheConfig.ways) bits)
    hitInvalidateWay := hitWay

    val invalidateWay = (cacheOp.cacheOp === CacheOp.indexInvalidateWriteBack) ? indexInvalidateWay | hitInvalidateWay
  }
  mem2.setName("mem2").reflectNames()

  val cacheOpPortsInit = new Area {
    for (i <- 0 until cacheConfig.ways) {
      val curRams = tagRams(i)
      val curWay = U(i, log2Up(cacheConfig.ways) bits)
      curRams.io.ena := mem1.cacheOp.en
      curRams.io.wea := B(0, 1 bits)
      curRams.io.addra := mem1.cacheOp.index
      curRams.io.dina := U(0, cacheConfig.tagWidth bits)
    }
    for (i <- 0 until cacheConfig.ways) {
      val curRam = validRams(i)
      val curWay = U(i, log2Up(cacheConfig.ways) bits)
      val cacheOpStateBootInvalidateValid = (mem2.cacheOp.cacheOp === CacheOp.indexInvalidateWriteBack) ? io.cacheOp.ready | (io.cacheOp.ready && mem2.hit)
      val cacheOpStateBootInvalidate = mem2.cacheOp.en && cacheOpStateBootInvalidateValid && (mem2.invalidateWay === curWay)
      curRam.io.ena := cacheOpStateBootInvalidate || mem1.cacheOp.en
      curRam.io.wea := cacheOpStateBootInvalidate ? B(1, 1 bits) | B(0, 1 bits)
      curRam.io.addra := cacheOpStateBootInvalidate ? mem2.cacheOp.index | mem1.cacheOp.index
      curRam.io.dina := U(0, 1 bits)
    }
    for (i <- 0 until cacheConfig.words) {
      for (j <- 0 until cacheConfig.ways) {
        val curRam = dataRams(i)(j)
        val curWay = U(j, log2Up(cacheConfig.ways) bits)
        curRam.io.ena := mem1.cacheOp.en
        curRam.io.wea := B(0, 4 bits)
        curRam.io.addra := mem1.cacheOp.index
        curRam.io.dina := U(0, 32 bits)
      }
    }
  }

  val if1 = new Area {
    val fetch = new Bundle {
      val flush = Bool()
      val exception = Bool()

      val en = Bool()
      val we = Bits(4 bits)
      val addr = UInt(32 bits)

      val cached = Bool()
      val correctTag = UInt(cacheConfig.tagWidth bits)

      val index = UInt(cacheConfig.indexWidth bits)
      val offset = UInt(cacheConfig.offsetWidth bits)
    }

    fetch.flush := io.fetch.flush
    fetch.exception := io.fetch.exception

    fetch.en := io.fetch.en
    fetch.we := io.fetch.we
    fetch.addr := io.fetch.addr

    fetch.cached := io.fetch.cached
    fetch.correctTag := io.fetch.correctTag
    fetch.index := fetch.addr((cacheConfig.indexWidth + cacheConfig.offsetWidth - 1) downto cacheConfig.offsetWidth)
    fetch.offset := fetch.addr((cacheConfig.offsetWidth - 1) downto 0)

    // random replace way generation, sequential
    val lfsrWidth = log2Up(cacheConfig.ways) + 2
    val lfsr = new Lfsr(lfsrWidth)
    lfsr.io.en := io.fetch.en & !io.fetch.isStalled
    lfsr.io.seed := U((lfsrWidth - 1 downto 0) -> true)
    // actually if2.lfsrDout, put it here in order to avoid recursive definition
    val lfsrDout = UInt(lfsrWidth bits)
    lfsrDout := lfsr.io.dout
  }
  if1.setName("if1").reflectNames()

  val if2 = new Area {
    val fetch = new Bundle {
      val en = RegInit(False)
      val we = RegInit(B(0, 4 bits))
      val addr = RegInit(U(0, 32 bits))

      val cached = RegInit(False)
      val correctTag = RegInit(U(0, cacheConfig.tagWidth bits))

      val index = RegInit(U(0, cacheConfig.indexWidth bits))
      val offset = RegInit(U(0, cacheConfig.offsetWidth bits))
    }

    when(!io.fetch.isStalled) {
      fetch.en := if1.fetch.en && !if1.fetch.flush && !if1.fetch.exception
      fetch.we := if1.fetch.we
      fetch.addr := if1.fetch.addr

      fetch.cached := if1.fetch.cached
      fetch.correctTag := if1.fetch.correctTag

      fetch.index := if1.fetch.index
      fetch.offset := if1.fetch.offset
    }

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
      hits(i) := valids(i) && (tags(i) === fetch.correctTag)
    }
    val hit = hits.orR
    val hitWay = MuxOH(
      hits,
      for (i <- 0 until cacheConfig.ways) yield {
        U(i, log2Up(cacheConfig.ways) bits)
      }
    )

    val dataLines = Vec(Vec(UInt(32 bits), cacheConfig.ways), cacheConfig.words)
    for (i <- 0 until cacheConfig.words) {
      for (j <- 0 until cacheConfig.ways) {
        val curRam = dataRams(i)(j)
        dataLines(i)(j) := curRam.io.doutb
      }
    }

    val data = Vec(UInt(32 bits), cacheConfig.ways)
    for (i <- 0 until cacheConfig.ways) {
      data(i) := dataLines(fetch.offset((cacheConfig.offsetWidth - 1) downto 2))(i)
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

    val replaceWay = invalid.mux(
      False -> if1.lfsrDout(log2Up(cacheConfig.ways) - 1 downto 0),
      True -> invalidWay
    )
    val replaceWayReg = RegInit(U(0, log2Up(cacheConfig.ways) bits))
    val replaceWayTagReg = RegInit(U(0, cacheConfig.tagWidth bits))
    val replaceWayDataReg = Vec(RegInit(U(0, 32 bits)), cacheConfig.words)

    val ibusInit = new Area {
      import io.ibus._
      ar.addr := fetch.cached ? (fetch.correctTag @@ fetch.index @@ U(0, cacheConfig.offsetWidth bits)) | fetch.addr// TODO maybe physical address error
      ar.id := U(0, 4 bits)
      ar.len := fetch.cached ? U(cacheConfig.words - 1, 8 bits) | U(0, 8 bits)
      ar.size := U(2, 3 bits) // 4 bytes
      ar.burst := fetch.cached ? B(1, 2 bits) | B(0, 2 bits) // cached INCR uncached FIXED
      // ar.lock := B(0, 2 bits)
      ar.cache := B(0, 4 bits)
      ar.prot := B(0, 3 bits)
    }
    val reLife = RegInit(False)
    val count = RegInit(U(0, log2Up(cacheConfig.words) bits))
    io.fetch.isStalled := True
    io.fetch.ready := False
    io.fetch.dout := U(0, 32 bits)
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
          replaceWayTagReg := fetch.correctTag
          goto(readNewR)
        }
      }

      readNewR.whenIsActive {
        r.ready := True
        when(r.valid) {
          replaceWayDataReg(count) := r.data.asUInt
          count := count + 1
          when(r.last) {
            readNewReady := True
            goto(stateBoot)
          }
        }
      }
    }
    val floodFillValid = CombInit(False)
    val floodFillReady = CombInit(False)
    val instCachedFSM = new StateMachine {
      setEntry(stateBoot)
      disableAutoStart()

      val readNew = new State()
      val floodFill = new State()

      stateBoot.whenIsActive {
        when(fetch.cached) {
          // combination
          when(reLife) {
            io.fetch.ready := True
            io.fetch.isStalled := False
          }.otherwise {
            io.fetch.ready := fetch.en && hit
            io.fetch.isStalled := fetch.en && !hit
          }

          // sequential
          when(io.fetch.isStalled) {
            // sequential
            reLife := True
            replaceWayReg := replaceWay
            // combination
            readNewValid := True
            goto(readNew)
          }.otherwise {
            reLife := False
            io.fetch.dout := fetch.en ? (
              reLife ? replaceWayDataReg(fetch.offset((cacheConfig.offsetWidth - 1) downto 2)) | data(hitWay)
            ) | U(0, 32 bits)
          }
        }
      }

      readNew.whenIsActive {
        io.fetch.ready := False
        io.fetch.isStalled := True
        when(readNewReady) {
          goto(floodFill)
        }
      }

      floodFill.whenIsActive {
        io.fetch.ready := False
        io.fetch.isStalled := True

        // using this to avoid 2 ports write in the same time
        floodFillValid := True
        floodFillReady := floodFillValid && !(mem2.cacheOp.en && io.cacheOp.ready)

        when(floodFillReady) {
          goto(stateBoot)
        }
      }
    }

    val uncachedData = RegInit(U(0, 32 bits))
    val instUncachedReadValid = CombInit(False)
    val instUncachedReadReady = CombInit(False)
    val instUncachedReadFSM = new StateMachine {
      import io.ibus._

      setEntry(stateBoot)
      disableAutoStart()

      val readAr = new State()
      val readR = new State()

      stateBoot.whenIsActive {
        when(instUncachedReadValid) {
          goto(readAr)
        }
      }

      readAr.whenIsActive {
        ar.valid := True
        when(ar.ready) {
          count := 0
          goto(readR)
        }
      }

      readR.whenIsActive {
        r.ready := True
        when(r.valid) {
          uncachedData := r.data.asUInt
          when(r.last) {
            instUncachedReadReady := True
            goto(stateBoot)
          }
        }
      }
    }
    val instUncachedFSM = new StateMachine {
      setEntry(stateBoot)
      disableAutoStart()

      val read = new State()

      stateBoot.whenIsActive {
        when(!fetch.cached) {
          // combination
          when(reLife) {
            io.fetch.ready := True
            io.fetch.isStalled := False
          }.otherwise {
            io.fetch.ready := False
            io.fetch.isStalled := fetch.en
          }

          // sequential
          when(io.fetch.isStalled) {
            reLife := True
            when(fetch.en) {
              instUncachedReadValid := True
              goto(read)
            }
          }.otherwise {
            reLife := False
            io.fetch.dout := fetch.en ? uncachedData | U(0, 32 bits)
          }
        }
      }

      read.whenIsActive {
        io.fetch.ready := False
        io.fetch.isStalled := True

        when(instUncachedReadReady) {
          goto(stateBoot)
        }
      }
    }
    instUncachedFSM.reflectNames()
  }
  if2.setName("if2").reflectNames()

  val ifPortsInit = new Area {
    for (i <- 0 until cacheConfig.ways) {
      val curRam = tagRams(i)
      val curWay = U(i, log2Up(cacheConfig.ways) bits)
      val cachedFloodFill = if2.floodFillReady && (if2.replaceWayReg === curWay)
      curRam.io.enb := cachedFloodFill || if1.fetch.en
      curRam.io.web := cachedFloodFill ? B(1, 1 bits) | B(0, 1 bits)
      curRam.io.addrb := cachedFloodFill ? if2.fetch.index | if1.fetch.index
      curRam.io.dinb := if2.replaceWayTagReg
    }

    for (i <- 0 until cacheConfig.ways) {
      val curRam = validRams(i)
      val curWay = U(i, log2Up(cacheConfig.ways) bits)
      val cachedFloodFill = if2.floodFillReady && (if2.replaceWayReg === curWay)
      curRam.io.enb := cachedFloodFill || if1.fetch.en
      curRam.io.web := cachedFloodFill ? B(1, 1 bits) | B(0, 1 bits)
      curRam.io.addrb := cachedFloodFill ? if2.fetch.index | if1.fetch.index
      curRam.io.dinb := U(1, 1 bits)
    }

    for (i <- 0 until cacheConfig.words) {
      for (j <- 0 until cacheConfig.ways) {
        val curRam = dataRams(i)(j)
        val curWay = U(j, log2Up(cacheConfig.ways) bits)
        val cachedFloodFill = if2.floodFillReady && (if2.replaceWayReg === curWay)
        curRam.io.enb := cachedFloodFill || if1.fetch.en
        curRam.io.web := cachedFloodFill ? B((3 downto 0) -> True) | B(0, 4 bits)
        curRam.io.addrb := cachedFloodFill ? if2.fetch.index | if1.fetch.index
        curRam.io.dinb := if2.replaceWayDataReg(i)
      }
    }
  }

  // TODO: maybe i cache inst need to flush all instrument after it in ISA
}

object ICacheGen {
  def main(args: Array[String]): Unit = {
    val spinalConfig = SpinalConfig(
      targetDirectory = "hw/gen",
      defaultConfigForClockDomains = J1cpuConfig().clockConfig
    )

    spinalConfig.generateVerilog(new ICache(CacheConfig(ways = 2, lines = 256, blockSize = 32), J1cpuConfig().axiConfig, 0))
  }
}
