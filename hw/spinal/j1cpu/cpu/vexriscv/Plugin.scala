package j1cpu.cpu.vexriscv

import spinal.core._
import spinal.lib._
import scala.collection.mutable

trait Plugin[T <: Pipeline] extends Nameable {
  var pipeline: T = null.asInstanceOf[T]

  def setup(pipeline: T) = {}

  def build(pipeline: T) = {}

  implicit class implicitsStage(stage: Stage) {
    def plug[T <: Area](area: T): T = {
      return area
    }
  }

  implicit class implicitsPipeline(stage: Pipeline) {
    def plug[T <: Area](area: T): T = {
      return area
    }
  }
}