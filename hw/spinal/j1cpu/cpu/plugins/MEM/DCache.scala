package j1cpu.cpu.plugins.mem

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axi.{Axi4, Axi4Config}
import spinal.lib.fsm.{State, StateMachine}
import j1cpu.cpu.{CacheConfig, J1cpu, J1cpuConfig}
import j1cpu.cpu.utils.{Bram, Dram, Lfsr}
import j1cpu.cpu.vexriscv.Plugin

class DCache(cacheConfig: CacheConfig, axiConfig: Axi4Config, sim: Int) extends Component {
  val io = new Bundle {
    val clk = in Bool()
    val reset = in Bool()

    // cpu
    val en = in Bool() // mem1
    val we = in UInt(4 bits) // mem1
    val addr = in UInt(32 bits) // mem1
    val din = in UInt(32 bits) // mem1
    val dout = out UInt(32 bits) // mem2

    // tlb
    val correctTag = in UInt(cacheConfig.tagWidth bits) // mem2

    // communicate signal for pipeline pass
    // ready recommendation:
    // mem2 stage en == 1: hit = 1 ready = 1
    // mem2 stage en == 1: hit = 0 wait until fsm back to idle to set ready = 1
    // mem2 stage en == 0: ready = 1
    val ready = out Bool() // mem2

    // axi
    val dbus = master(Axi4(axiConfig)).setIdle() // mem2
  }
  noIoPrefix()

  io.dbus.flattenForeach {
    signal => {
      val tmpName = signal.getName()
      val tmpList = tmpName.split("_")
      signal.setName(tmpList(0) + "_" + tmpList(1) + "_" + tmpList(tmpList.size - 1))
    }
  }

  new ClockingArea(
    new ClockDomain(
      clock = io.clk,
      reset = io.reset,
      config = ClockDomainConfig(
        resetActiveLevel = HIGH,
        resetKind = SYNC,
        clockEdge = RISING
      )
    )
  ) {
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
      val en = Bool()
      val we = UInt(4 bits)
      val addr = UInt(32 bits)
      val din = UInt(32 bits)

      val correctTag = UInt(cacheConfig.tagWidth bits)
      val index = UInt(cacheConfig.indexWidth bits)
      val offset = UInt(cacheConfig.offsetWidth bits)

      en := io.en
      we := io.we
      addr := io.addr
      din := io.din

      correctTag := io.correctTag
      index := addr((cacheConfig.indexWidth + cacheConfig.offsetWidth - 1) downto cacheConfig.offsetWidth)
      offset := addr((cacheConfig.offsetWidth - 1) downto 0)

      // mem1 read, mem1 receive result
      val tags = Vec(UInt(cacheConfig.tagWidth bits), cacheConfig.ways)
      for (i <- 0 until cacheConfig.ways) {
        val curRam = tagRams(i)
        curRam.io.clk := io.clk

        curRam.io.ena := False
        curRam.io.wea := U(0, 1 bits)
        curRam.io.addra := U(0, cacheConfig.indexWidth bits)
        curRam.io.dina := U(0, cacheConfig.tagWidth bits)

        curRam.io.enb := en
        curRam.io.addrb := index
        tags(i) := curRam.io.doutb
      }

      val valids = Vec(Bool(), cacheConfig.ways)
      for (i <- 0 until cacheConfig.ways) {
        val curRam = validRams(i)
        curRam.io.clk := io.clk

        curRam.io.ena := False
        curRam.io.wea := U(0, 1 bits)
        curRam.io.addra := U(0, cacheConfig.indexWidth bits)
        curRam.io.dina := U(0, 1 bits)

        curRam.io.enb := en
        curRam.io.addrb := index
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
          curRam.io.clk := io.clk

          curRam.io.ena := False
          curRam.io.wea := U(0, 4 bits)
          curRam.io.addra := index
          curRam.io.dina := U(0, 32 bits)

          curRam.io.enb := en
          curRam.io.addrb := index
          dataLines(i)(j) := curRam.io.doutb
        }
      }

      val dirtys = Vec(Bool(), cacheConfig.ways)
      for (i <- 0 until cacheConfig.ways) {
        val curRam = dirtyRams(i)
        curRam.io.clk := io.clk

        curRam.io.ena := False
        curRam.io.wea := U(0, 1 bits)
        curRam.io.addra := U(0, cacheConfig.indexWidth bits)
        curRam.io.dina := U(0, 1 bits)

        curRam.io.enb := en
        curRam.io.addrb := index
        dirtys(i) := curRam.io.doutb.asBool
      }

      // random replace way generation, sequential
      val lfsrWidth = log2Up(cacheConfig.ways) + 2
      val lfsr = new Lfsr(lfsrWidth)
      lfsr.io.clk := io.clk
      lfsr.io.reset := io.reset
      lfsr.io.en := io.en & io.ready
      lfsr.io.seed := U((lfsrWidth - 1 downto 0) -> true)
      // actually mem2.lfsrDout, put it here in order to avoid recursive definition
      val lfsrDout = UInt(lfsrWidth bits)
      lfsrDout := lfsr.io.dout
    }
    mem1.setName("mem1").reflectNames()

    val mem2 = new Area {
      val en = RegInit(False)
      val we = RegInit(U(0, 4 bits))
      val addr = RegInit(U(0, 32 bits))
      val din = RegInit(U(0, 32 bits))

      val tags = Vec(RegInit(U(0, cacheConfig.tagWidth bits)), cacheConfig.ways)
      val correctTag = RegInit(U(0, cacheConfig.tagWidth bits))

      val valids = Vec(RegInit(False), cacheConfig.ways)

      val index = RegInit(U(0, cacheConfig.indexWidth bits))
      val offset = RegInit(U(0, cacheConfig.offsetWidth bits))

      val dirtys = Vec(RegInit(False), cacheConfig.ways)

      when(io.ready) {
        en := mem1.en
        we := mem1.we
        addr := mem1.addr
        din := mem1.din

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
        data(i) := U(0, 32 bits)
      }
      for (i <- 0 until cacheConfig.words) {
        when(offset((cacheConfig.offsetWidth - 1) downto 2) === U(i, (cacheConfig.offsetWidth - 2) bits)) {
          for (j <- 0 until cacheConfig.ways) {
            data(j) := mem1.dataLines(i)(j)
          }
        }
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
        False -> mem1.lfsrDout(log2Up(cacheConfig.ways) - 1 downto 0),
        True -> invalidWay
      )
      val replaceWayReg = RegInit(U(0, log2Up(cacheConfig.ways) bits))
      val replaceWayTag = tags(replaceWay)
      val replaceWayTagReg = RegInit(U(0, cacheConfig.tagWidth bits))
      val replaceWayData = Vec(UInt(32 bits), cacheConfig.words)
      val replaceWayDataReg = Vec(RegInit(U(0, 32 bits)), cacheConfig.words)
      for (i <- 0 until cacheConfig.words) {
        replaceWayData(i) := mem1.dataLines(i)(replaceWay)
      }

      // we use five state to describe data cache's operation when cache miss
      // idle state is the initial state, at this state we compare virtual address and tag
      // writeBack state use when the cache line is dirty, we write back it to memory via axi
      // readNew state we read cache line from memory via axi
      // floodFill state we replace the cache line with new cache line
      // update state we update the previous error tag and data
      // after that we back to idle state, compare current correct tag and data and enter next stage
      val reLife = RegInit(False)
      val count = RegInit(U(0, log2Up(cacheConfig.words) bits))
      io.ready := False
      io.dout := U(0, 32 bits)
      val dataCacheMainFSM = new StateMachine {
        import io.dbus._

        val idle = new State()
        setEntry(idle)

        // writeBack
        val writeBackAw = new State()
        val writeBackW = new State()
        val writeBackB = new State()

        // readNew
        val readNewAr = new State()
        val readNewR = new State()

        val floodFill = new State()

        val update = new State()

        idle.whenIsActive {
          // combination
          when(reLife) {
            io.ready := True
          }.otherwise {
            io.ready := !en || (en && hit)
          }
          io.dout := data(hitWay)

          // sequential
          when(!io.ready) {
            reLife := True
            when(!dirtys(replaceWay)) {
              goto(readNewAr)
            }.otherwise {
              replaceWayReg := replaceWay
              replaceWayTagReg := replaceWayTag
              for (i <- 0 until cacheConfig.words) {
                replaceWayDataReg(i) := replaceWayData(i)
              }
              goto(writeBackAw)
            }
          }.otherwise {
            reLife := False
          }
        }

        writeBackAw.whenIsActive {
          io.ready := False

          aw.valid := True
          aw.addr := replaceWayTagReg @@ index @@ U(0, cacheConfig.offsetWidth bits) // TODO maybe physical address error
          aw.id := U(1, 5 bits);
          aw.len := U(cacheConfig.words - 1, 8 bits)
          aw.size := U(2, 3 bits) // 4 bytes each time
          aw.burst := B(1, 2 bits) // INCR
          aw.lock := B(0, 1 bits)
          aw.cache := B(0, 4 bits)
          aw.prot := B(0, 3 bits)
          when(aw.ready) {
            count := U(0, log2Up(cacheConfig.words) bits)
            goto(writeBackW)
          }
        }

        writeBackW.whenIsActive {
          io.ready := False

          w.valid := True
          w.data := replaceWayDataReg(count).asBits
          w.strb := B((3 downto 0) -> true)
          w.last := (count === U((log2Up(cacheConfig.words) - 1 downto 0) -> true))
          when(w.ready) {
            count := count + 1
            when(w.last) {
              b.ready := True
              goto(writeBackB)
            }
          }
        }

        writeBackB.whenIsActive {
          io.ready := False

          b.ready := True
          when(b.valid) {
            goto(readNewAr)
          }
        }

        readNewAr.whenIsActive {
          io.ready := True

          ar.valid := True
          ar.addr
          ar.id := U(1, 5 bits)
          ar.len := U(cacheConfig.words - 1, 8 bits)
          ar.size := U(2, 3 bits) // 4 bytes
          ar.burst := B(1, 2 bits) // INCR
          ar.lock := B(0, 1 bits)
          ar.cache := B(0, 4 bits)
          ar.prot := B(0, 3 bits)
          when(ar.ready) {
            count := 0
            goto(readNewR)
          }
        }

        readNewR.whenIsActive {
          io.ready := False

          r.ready := True
          when(r.valid) {
            replaceWayDataReg(count).asBits := r.data
            count := count + 1
            when(r.last && r.fire) {
              goto(floodFill)
            }
          }
        }

        floodFill.whenIsActive {
          io.ready := False

          for (i <- 0 until cacheConfig.words) {
            for (j <- 0 until cacheConfig.ways) {
              val curRam = dataRams(i)(j)
              curRam.io.ena := replaceWayReg === U(j, log2Up(cacheConfig.ways) bits)
              curRam.io.wea := U((3 downto 0) -> true)
              curRam.io.addra := index
              curRam.io.dina := replaceWayDataReg(i)
            }
          }

          for (i <- 0 until cacheConfig.ways) {
            val curRam = tagRams(i)
            curRam.io.ena := (replaceWayReg === U(i, log2Up(cacheConfig.ways) bits))
            curRam.io.wea := U(1, 1 bits)
            curRam.io.addra := index
            curRam.io.dina := replaceWayTagReg
          }

          for (i <- 0 until cacheConfig.ways) {
            val curRam = validRams(i)
            curRam.io.ena := (replaceWayReg === U(i, log2Up(cacheConfig.ways) bits))
            curRam.io.wea := U(1, 1 bits)
            curRam.io.addra := index
            curRam.io.dina := U(1, 1 bits)
          }

          for (i <- 0 until cacheConfig.ways) {
            val curRam = dirtyRams(i)
            curRam.io.ena := (replaceWayReg === U(i, log2Up(cacheConfig.ways) bits))
            curRam.io.wea := U(1, 1 bits)
            curRam.io.addra := index
            curRam.io.dina := U(0, 1 bits)
          }

          goto(update)
        }

        update.whenIsActive {
          io.ready := False

          for (i <- 0 until cacheConfig.words) {
            for (j <- 0 until cacheConfig.ways) {
              val curRam = dataRams(i)(j)
              curRam.io.enb := en
              curRam.io.addrb := index
            }
          }
          hit := True

          goto(idle)
        }
      }
      dataCacheMainFSM.reflectNames()
    }
    mem2.setName("mem2").reflectNames()
  }
}

object DCacheGen {
  def main(args: Array[String]): Unit = {
    val spinalConfig = SpinalConfig(
      targetDirectory = "hw/gen",
      defaultConfigForClockDomains = ClockDomainConfig(
        resetActiveLevel = HIGH,
        resetKind = SYNC,
        clockEdge = RISING
      )
    )

    spinalConfig.generateVerilog(new DCache(new CacheConfig(ways = 2, lines = 256, blockSize = 32), new J1cpuConfig().axiConfig, 0))
  }
}