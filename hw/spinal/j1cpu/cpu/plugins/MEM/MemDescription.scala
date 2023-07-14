package j1cpu.cpu.plugins.MEM

import j1cpu.cpu.plugins.ID.IdDescription
import j1cpu.cpu.signals.{MemWe, WbSrc}
import j1cpu.cpu.{J1cpu, J1cpuConfig}
import j1cpu.cpu.vexriscv.Plugin
import spinal.core._
import spinal.lib._

class MemDescription(config: J1cpuConfig) extends Plugin[J1cpu] {
  val dCache = new DCache(config.cacheConfig, config.axiConfig, config.sim)
  override def build(pipeline: J1cpu): Unit = {
    import pipeline._
    import pipeline.signal._

    val ByPassNetWorkService = service[IdDescription].byPassNetwork
    MEM1 plug new Area {
      import MEM1._
      pipelineSignal.flush := False
      pipelineSignal.stall := False

      dCache.io.flush := pipelineSignal.isFlushed
      dCache.io.en := pipelineSignal.isValid && !pipelineSignal.isStalled
      dCache.io.we := !input(MEM_W) ? B"0000" | input(MEM_WE).mux(
        MemWe.B -> input(MEM_ADDRESS)(1 downto 0).muxListDc(
          for (i <- 0 until 4) yield (i, B(4 bits, i -> True, default -> False))
        ),
        MemWe.H -> (input(MEM_ADDRESS)(0) ? B"1100" | B"0011"),
        MemWe.W -> B"1111",
        MemWe.WL -> input(MEM_ADDRESS)(1 downto 0).muxListDc(
          for (i <- 0 until 4) yield (i, B(4 bits, (i downto 0) -> True, default -> False))
        ),
        MemWe.WR -> input(MEM_ADDRESS)(1 downto 0).muxListDc(
          for (i <- 0 until 4) yield (i, B(4 bits, (3 downto i) -> True, default -> False))
        ),
        default -> B"0000"
      )
      dCache.io.addr := input(MEM_ADDRESS)
      dCache.io.din := input(MEM_WE).mux(
        MemWe.WL -> input(MEM_ADDRESS)(1 downto 0).muxListDc(
          for (i <- 0 until 4) yield (i, U(0, 8 * (3 - i) bits) @@ input(MEM_DIN)(31 downto (8 * (3 - i))))
        ),
        MemWe.WR -> input(MEM_ADDRESS)(1 downto 0).muxListDc(
          for (i <- 0 until 4) yield (i, input(MEM_DIN)(((4 - i) * 8 - 1) downto 0) @@ U(0, i * 8 bits))
        ),
        default -> input(MEM_DIN)
      )

      ByPassNetWorkService.io.sValid(1) := input(WB_EN)
      ByPassNetWorkService.io.sReady(1) := (input(WB_SRC) === WbSrc.Alu) && (input(WB_SRC) === WbSrc.Mdu)
      ByPassNetWorkService.io.sAddr(1) := input(WB_REG)
      ByPassNetWorkService.io.sDin(1) := input(WB_SRC).mux(
        WbSrc.Alu -> input(ALU_RESULT),
        default -> U(0, 32 bits)
      )
    }

    MEM2 plug new Area {
      import MEM2._
      pipelineSignal.flush := False
      pipelineSignal.stall := False

      val memDout = dCache.io.dout
      insert(MEM_RESULT) := input(MEM_WE).mux(
        MemWe.B -> input(MEM_ADDRESS)(1 downto 0).muxListDc(
          for (i <- 0 until  4) yield (i, B(24 bits, default -> memDout(i * 8 + 7)).asUInt @@ memDout((i * 8 + 7) downto (i * 8)))
        ),
        MemWe.BU -> input(MEM_ADDRESS)(1 downto 0).muxListDc(
          for (i <- 0 until 4) yield (i, U(0, 24 bits) @@ memDout((i * 8 + 7) downto (i * 8)))
        ),
        MemWe.H -> input(MEM_ADDRESS)(1 downto 1).muxListDc(
          for (i <- 0 until 2) yield (i, B(16 bits, default -> memDout(i * 16 + 15)).asUInt @@ memDout((i * 16 + 15) downto (i * 16)))
        ),
        MemWe.HU -> input(MEM_ADDRESS)(1 downto 1).muxListDc(
          for (i <- 0 until 2) yield (i, U(0, 16 bits) @@ memDout((i * 16 + 15) downto (i * 16)))
        ),
        MemWe.W -> memDout,
        MemWe.WL -> input(MEM_ADDRESS)(1 downto 0).muxListDc(
          for (i <- 0 until 4) yield (i, if (i == 3) memDout else memDout((i * 8 + 7) downto 0) @@ input(MEM_DIN)(((3 - i) * 8 - 1) downto 0))
        ),
        MemWe.WR -> input(MEM_ADDRESS)(1 downto 0).muxListDc(
          for (i <- 0 until 4) yield (i, if (i == 0) memDout else input(MEM_DIN)(31 downto ((4 - i) * 8)) @@ memDout(31 downto (i * 8)))
        )
      )

      ByPassNetWorkService.io.sValid(2) := input(WB_EN)
      ByPassNetWorkService.io.sAddr(2) := input(WB_REG)
      ByPassNetWorkService.io.sDin(2) := input(WB_SRC).mux(
        WbSrc.Alu -> input(ALU_RESULT),
        WbSrc.DCache -> input(MEM_RESULT),
        default -> U(0, 32 bits)
      )
    }

  }
}
