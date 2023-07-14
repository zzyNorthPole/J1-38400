package j1cpu.cpu.plugins.MEM

import j1cpu.cpu.signals.CacheOp
import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axi.{Axi4, Axi4Config}
import spinal.lib.fsm.{State, StateMachine}
import j1cpu.cpu.{CacheConfig, J1cpu, J1cpuConfig}
import j1cpu.cpu.utils.{Bram, Dram, Lfsr}
import j1cpu.cpu.vexriscv.Plugin

class DCache(cacheConfig: CacheConfig, axiConfig: Axi4Config, sim: Int) extends Component {
  val io = new Bundle {
    // cpu
    val flush = in Bool() // mem1

    val en = in Bool() // mem1
    val we = in Bits (4 bits) // mem1
    val addr = in UInt (32 bits) // mem1
    val din = in UInt (32 bits) // mem1
    val dout = out UInt (32 bits) // mem2

    // cache operation
    val cacheOpEn = in Bool()
    val cacheOp = in(CacheOp())

    // tlb
    val cached = in Bool() // mem1, used for uncached
    val correctTag = in UInt (cacheConfig.tagWidth bits) // mem1

    // communicate signal for pipeline pass
    // ready recommendation:
    // mem2 stage en == 1: hit = 1 ready = 1
    // mem2 stage en == 1: hit = 0 wait until fsm back to idle to set ready = 1
    // mem2 stage en == 0: ready = 1
    val ready = out Bool() // mem2
    val valid = out Bool() // mem2

    // axi
    val dbus = master(Axi4(axiConfig)).setIdle() // mem2
    val udbus = master(Axi4(axiConfig)).setIdle() // mem2
  }
  noIoPrefix()

  io.dbus.flattenForeach {
    signal => {
      val tmpName = signal.getName()
      val tmpList = tmpName.split("_")
      signal.setName(tmpList(0) + "_" + tmpList(1) + "_" + tmpList(tmpList.size - 1))
    }
  }

  io.udbus.flattenForeach {
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

  val dirtyRams = Seq.fill(cacheConfig.ways) {
    new Dram(cacheConfig.lines, 1, 0, sim)
  }

  // we use two stage to accomplish MEM operation load/store
  // MEM1: search in cache for the cache line, search in tlb/mmu for virtual address transform
  // MEM2: compare address and tag, if equal then goto to WB, else use a FSM to fix it

  val mem1 = new Area {
    val flush = Bool()

    val en = Bool()
    val we = Bits (4 bits)
    val addr = UInt (32 bits)
    val din = UInt (32 bits)

    val cacheOpEn = Bool()
    val cacheOp = CacheOp()

    val cached = Bool()
    val correctTag = UInt (cacheConfig.tagWidth bits)
    val index = UInt (cacheConfig.indexWidth bits)
    val offset = UInt (cacheConfig.offsetWidth bits)

    flush := io.flush

    en := io.en
    we := io.we
    addr := io.addr
    din := io.din

    cacheOpEn := io.cacheOpEn
    cacheOp := io.cacheOp

    cached := io.cached
    correctTag := io.correctTag
    index := addr((cacheConfig.indexWidth + cacheConfig.offsetWidth - 1) downto cacheConfig.offsetWidth)
    offset := addr((cacheConfig.offsetWidth - 1) downto 0)

    val rPortsInit = new Area {
      // query tag
      // mem1 read, mem1 receive result
      for (i <- 0 until cacheConfig.ways) {
        val curRam = tagRams(i)
        curRam.io.enb := en
        curRam.io.addrb := index
      }

      // query valid
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

      for (i <- 0 until cacheConfig.ways) {
        val curRam = dirtyRams(i)
        curRam.io.enb := en
        curRam.io.addrb := index
      }
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

    val dirtys = Vec(Bool(), cacheConfig.ways)
    for (i <- 0 until cacheConfig.ways) {
      val curRam = dirtyRams(i)
      dirtys(i) := curRam.io.doutb.asBool
    }

    // random replace way generation, sequential
    val lfsrWidth = log2Up(cacheConfig.ways) + 2
    val lfsr = new Lfsr(lfsrWidth)
    lfsr.io.en := io.en & io.ready
    lfsr.io.seed := U((lfsrWidth - 1 downto 0) -> true)
    // actually mem2.lfsrDout, put it here in order to avoid recursive definition
    val lfsrDout = UInt(lfsrWidth bits)
    lfsrDout := lfsr.io.dout
  }
  mem1.setName("mem1").reflectNames()

  val mem2 = new Area {
    val en = RegInit(False)
    val we = RegInit(B(0, 4 bits))
    val addr = RegInit(U(0, 32 bits))
    val din = RegInit(U(0, 32 bits))

    val cacheOpEn = RegInit(False)
    val cacheOp = RegInit(CacheOp.indexInvalidateWriteBack)

    val cached = RegInit(False)

    val tags = Vec(RegInit(U(0, cacheConfig.tagWidth bits)), cacheConfig.ways)
    val correctTag = RegInit(U(0, cacheConfig.tagWidth bits))

    val valids = Vec(RegInit(False), cacheConfig.ways)

    val index = RegInit(U(0, cacheConfig.indexWidth bits))
    val offset = RegInit(U(0, cacheConfig.offsetWidth bits))

    val dirtys = Vec(RegInit(False), cacheConfig.ways)

    when(io.ready) {
      en := mem1.en && !mem1.flush

      we := mem1.we
      addr := mem1.addr
      din := mem1.din

      cacheOpEn := mem1.cacheOpEn
      cacheOp := mem1.cacheOp

      cached := mem1.cached

      tags := mem1.tags
      correctTag := mem1.correctTag

      valids := mem1.valids

      index := mem1.index
      offset := mem1.offset

      dirtys := mem1.dirtys
    }

    // hit
    val hit = RegInit(False)
    val hitWay = RegInit(U(0, log2Up(cacheConfig.ways) bits))
    when(io.ready) {
      hit := mem1.hit
      hitWay := mem1.hitWay
    }

    val data = Vec(UInt(32 bits), cacheConfig.ways)
    for (i <- 0 until cacheConfig.ways) {
      data(i) := mem1.dataLines(offset((cacheConfig.offsetWidth - 1) downto 2))(i)
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
    val indexInvalidateWay = UInt (log2Up(cacheConfig.ways) bits)
    indexInvalidateWay := correctTag(log2Up(cacheConfig.ways) - 1 downto 0)
    // hit invalidate way of cache operation
    val hitInvalidateWay = UInt (log2Up(cacheConfig.ways) bits)
    hitInvalidateWay := hitWay

    val replaceWay = cacheOpEn.mux(
      False -> invalid.mux(
        False -> mem1.lfsrDout(log2Up(cacheConfig.ways) - 1 downto 0),
        True -> invalidWay
      ),
      True -> (cacheOp === CacheOp.indexInvalidateWriteBack).mux(
        False -> hitInvalidateWay,
        True -> indexInvalidateWay
      )
    )
    val replaceWayReg = RegInit(U(0, log2Up(cacheConfig.ways) bits))
    val replaceWayTag = tags(replaceWay)
    val replaceWayTagReg = RegInit(U(0, cacheConfig.tagWidth bits))
    val replaceWayData = Vec(UInt(32 bits), cacheConfig.words)
    val replaceWayDataReg = Vec(RegInit(U(0, 32 bits)), cacheConfig.words)
    for (i <- 0 until cacheConfig.words) {
      replaceWayData(i) := mem1.dataLines(i)(replaceWay)
    }

    // we use four state to describe data cache's operation when cache miss
    // idle state is the initial state, if current way hits, return load data from hit way or store data in hit way
    // writeBack state use when the cache line is dirty, we write back it to memory via axi
    // readNew state we read cache line from memory via axi
    // floodFill state we replace the cache line with new cache line
    // after that we back to idle state, return load data from replace way data or store data in replace way
    val reLife = RegInit(False)
    val count = RegInit(U(0, log2Up(cacheConfig.words) bits))
    io.ready := False
    io.valid := False
    io.dout := U(0, 32 bits)
    val dbusInit = new Area {
      import io.dbus._
      aw.addr := replaceWayTagReg @@ index @@ U(0, cacheConfig.offsetWidth bits) // TODO maybe physical address error
      aw.id := U(1, 5 bits);
      aw.len := U(cacheConfig.words - 1, 8 bits)
      aw.size := U(2, 3 bits) // 4 bytes each time
      aw.burst := B(1, 2 bits) // INCR
      aw.lock := B(0, 1 bits)
      aw.cache := B(0, 4 bits)
      aw.prot := B(0, 3 bits)

      w.data := replaceWayDataReg(count).asBits
      w.strb := B((3 downto 0) -> true)
      w.last := (count === U((log2Up(cacheConfig.words) - 1 downto 0) -> true))

      ar.addr := correctTag @@ index @@ U(0, cacheConfig.offsetWidth bits) // TODO maybe physical address error
      ar.id := U(1, 5 bits)
      ar.len := U(cacheConfig.words - 1, 8 bits)
      ar.size := U(2, 3 bits) // 4 bytes
      ar.burst := B(1, 2 bits) // INCR
      ar.lock := B(0, 1 bits)
      ar.cache := B(0, 4 bits)
      ar.prot := B(0, 3 bits)
    }
    val writeBackValid = CombInit(False)
    val writeBackReady = CombInit(False)
    val dataCachedWriteBackFSM = new StateMachine {
      import io.dbus._

      setEntry(stateBoot)
      disableAutoStart()

      val writeBackAw = new State()
      val writeBackW = new State()
      val writeBackB = new State()

      stateBoot.whenIsActive {
        when(writeBackValid) {
          goto(writeBackAw)
        }
      }

      writeBackAw.whenIsActive {
        aw.valid := True
        when(aw.ready) {
          count := U(0, log2Up(cacheConfig.words) bits)
          goto(writeBackW)
        }
      }

      writeBackW.whenIsActive {
        w.valid := True
        when(w.ready) {
          count := count + 1
          when(w.last) {
            b.ready := True
            goto(writeBackB)
          }
        }
      }

      writeBackB.whenIsActive {
        b.ready := True
        when(b.valid) {
          writeBackReady := True
          goto(stateBoot)
        }
      }
    }
    val readNewValid = CombInit(False)
    val readNewReady = CombInit(False)
    val dataCachedReadNewFSM = new StateMachine {
      import io.dbus._

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
    val dataCachedFSM = new StateMachine {
      setEntry(stateBoot)
      disableAutoStart()

      val writeBack = new State()
      val readNew = new State()
      val floodFill = new State()

      stateBoot.whenIsActive {
        when(cached && !cacheOpEn) {
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
            reLife := True
            replaceWayReg := replaceWay
            when(!valids(replaceWay) || !dirtys(replaceWay)) {
              readNewValid := True
              goto(readNew)
            }.otherwise {
              replaceWayTagReg := replaceWayTag
              for (i <- 0 until cacheConfig.words) {
                replaceWayDataReg(i) := replaceWayData(i)
              }
              writeBackValid := True
              goto(writeBack)
            }
          }.otherwise {
            reLife := False
            io.dout := reLife ? replaceWayDataReg(offset(cacheConfig.offsetWidth - 1 downto 2)) | data(hitWay)
          }
        }
      }

      writeBack.whenIsActive {
        io.valid := False
        io.ready := False

        when(writeBackReady) {
          readNewValid := True
          goto(readNew)
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
    dataCachedFSM.reflectNames()

    // TODO: cache operation need to be added after decoder and cp0 generation
    // we reuse write back fsm in dataCacheFSM
    // when data cache operation added, following things need to be changed:
    // 1. io.ready/io.valid
    // 2. replaceWay
    // 3. valid ram update logic
    // in this cpu, currently consider about three situation:
    // 1. index invalidate with write back
    // 2. hit invalidate with write back
    // 3. hit invalidate without write back
    // the following signals we should consider about:
    // 1. cached && cacheOpEn: start dataCacheOpFSM
    // 2. cacheOp
    // 3. en: only en == true the cache operation valid
    // 4. valid
    // 5. hit
    // 6. dirty
    // cacheOp == index invalidate with write back:
    // hit is useless as we use correctTag to gen write back way
    // when valid == false, the operation finished
    // when valid == true and dirty == false, only need to set valid rams
    // when valid == true and dirty == false, first write back to ram and then set valid rams
    // cacheOp == hit invalidate with write back:
    // when hit == false, the operation finished as nothing should be done
    // when hit == true and valid == false, the operation finished
    // when hit == true and valid == true and dirty == false, only need to set valid rams
    // when hit == true and valid == true and dirty == true, first write back to ram and then set valid rams
    // cacheOp == hit invalidate without write back:
    // when hit == false, the operation finished as nothing should be done
    // when hit == false, the operation finished as nothing should be done
    // when hit == true and valid == false, the operation finished
    // when hit == true and valid == true and dirty == false, only need to set valid rams
    // when hit == true and valid == true and dirty == true, only need to set valid rams
    val dataCacheOpFSM = new StateMachine {
      import CacheOp._

      setEntry(stateBoot)
      disableAutoStart()

      val writeBack = new State()

      stateBoot.whenIsActive {
        when(cached && cacheOpEn) {
          // combination
          when(reLife) {
            io.valid := True
            io.ready := True
          }.otherwise {
            io.valid := cacheOp.mux(
              indexInvalidateWriteBack -> (en && !(valids(indexInvalidateWay) && dirtys(indexInvalidateWay))),
              hitInvalidateWriteBack -> (en && !(hit && valids(hitInvalidateWay) && dirtys(hitInvalidateWay))),
              default -> en
            )
            io.ready := !en || io.valid
          }

          // sequential
          when(!io.ready) {
            reLife := True
            replaceWayReg := replaceWay
            replaceWayTagReg := replaceWayTag
            for (i <- 0 until cacheConfig.words) {
              replaceWayDataReg(i) := replaceWayData(i)
            }
            writeBackValid := True
            goto(writeBack)
          }.otherwise {
            reLife := False
          }
        }
      }

      writeBack.whenIsActive {
        when(writeBackReady) {
          goto(stateBoot)
        }
      }
    }

    // TODO: load/store instrument operator and dirty fix
    val writeWay = reLife ? replaceWayReg | hitWay
    val invalidateWay = reLife ? replaceWayReg | (
      (cacheOp === CacheOp.indexInvalidateWriteBack) ? indexInvalidateWay | hitInvalidateWay
    )
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
        val cacheOpStateBootInvalidate = cached && cacheOpEn && cacheOpStateBootInvalidateValid && (invalidateWay === curWay)
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
          val cachedStateBootWrite = cached && !cacheOpEn && io.valid && (writeWay === curWay)
          curRam.io.ena := cachedFloodFill || cachedStateBootWrite
          curRam.io.wea := cachedFloodFill ? B((3 downto 0) -> true) | we
          curRam.io.addra := index
          curRam.io.dina := cachedFloodFill ? replaceWayDataReg(i) | din
        }
      }

      for (i <- 0 until cacheConfig.ways) {
        val curRam = dirtyRams(i)
        val curWay = U(i, log2Up(cacheConfig.ways) bits)
        val cachedFloodFill = floodFillValid && (replaceWayReg === curWay)
        val cachedStateBootWrite = cached && !cacheOpEn && io.valid && (writeWay === curWay)
        curRam.io.ena := cachedFloodFill || cachedStateBootWrite
        curRam.io.wea := B(1, 1 bits)
        curRam.io.addra := index
        curRam.io.dina := cachedFloodFill ? U(0, 1 bits) | U(1, 1 bits)
      }
    }

    val uncachedData = RegInit(U(0, 32 bits))
    val udbusInit = new Area {
      import io.udbus._
      aw.addr := correctTag @@ index @@ U(0, cacheConfig.offsetWidth bits) // TODO maybe physical address error
      aw.id := U(1, 5 bits);
      aw.len := U(0, 8 bits)
      aw.size := U(2, 3 bits) // 4 bytes each time
      aw.burst := B(0, 2 bits) // FIXED
      aw.lock := B(0, 1 bits)
      aw.cache := B(0, 4 bits)
      aw.prot := B(0, 3 bits)

      w.data := din.asBits
      w.strb := we
      w.last := True

      ar.addr := correctTag @@ index @@ U(0, cacheConfig.offsetWidth bits) // TODO maybe physical address error
      ar.id := U(1, 5 bits)
      ar.len := U(0, 8 bits)
      ar.size := U(2, 3 bits) // 4 bytes
      ar.burst := B(0, 2 bits) // FIXED
      ar.lock := B(0, 1 bits)
      ar.cache := B(0, 4 bits)
      ar.prot := B(0, 3 bits)
    }
    val dataUncachedWriteValid = CombInit(False)
    val dataUncachedWriteReady = CombInit(False)
    val dataUncachedWriteFSM = new StateMachine {
      import io.udbus._

      setEntry(stateBoot)
      disableAutoStart()

      val writeAw = new State()
      val writeW = new State()
      val writeB = new State()

      stateBoot.whenIsActive {
        when(dataUncachedWriteValid) {
          goto(writeAw)
        }
      }

      writeAw.whenIsActive {
        aw.valid := True
        when(aw.ready) {
          count := U(0, log2Up(cacheConfig.words) bits)
          goto(writeW)
        }
      }

      writeW.whenIsActive {
        w.valid := True
        when(w.ready) {
          b.ready := True
          goto(writeB)
        }
      }

      writeB.whenIsActive {
        b.ready := True
        when(b.valid) {
          dataUncachedWriteReady := True
          goto(stateBoot)
        }
      }
    }
    val dataUncachedReadValid = CombInit(False)
    val dataUnCachedReadReady = CombInit(False)
    val dataUncachedReadFSM = new StateMachine {
      import io.udbus._

      setEntry(stateBoot)
      disableAutoStart()

      val readAr = new State()
      val readR = new State()

      stateBoot.whenIsActive {
        when(dataUncachedReadValid) {
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
          when(r.fire) {
            dataUnCachedReadReady := True
            goto(stateBoot)
          }
        }
      }
    }
    val dataUncachedFSM = new StateMachine {
      setEntry(stateBoot)
      disableAutoStart()

      val write = new State()
      val read = new State()

      stateBoot.whenIsActive {
        when(!cached) {
          // combination
          when(reLife) {
            io.valid := True
            io.ready := True
          }.otherwise {
            io.valid := False
            io.ready := !en
          }

          // sequential
          when(!io.ready) {
            reLife := True
            when(en) {
              when(we.orR) {
                dataUncachedWriteValid := True
                goto(write)
              }.otherwise {
                dataUncachedWriteValid := True
                goto(read)
              }
            }
          }.otherwise {
            reLife := False
            io.dout := uncachedData
          }
        }
      }

      write.whenIsActive {
        io.valid := False
        io.ready := False

        when(dataUncachedWriteReady) {
          goto(stateBoot)
        }
      }

      read.whenIsActive {
        io.valid := False
        io.ready := False

        when(dataUnCachedReadReady) {
          goto(stateBoot)
        }
      }
    }
    dataUncachedFSM.reflectNames()
  }
  mem2.setName("mem2").reflectNames()
}

object DCacheGen {
  def main(args: Array[String]): Unit = {
    val spinalConfig = SpinalConfig(
      targetDirectory = "hw/gen",
      defaultConfigForClockDomains = new J1cpuConfig().clockConfig
    )

    spinalConfig.generateVerilog(new DCache(CacheConfig(ways = 2, lines = 256, blockSize = 32), J1cpuConfig().axiConfig, 0))
  }
}