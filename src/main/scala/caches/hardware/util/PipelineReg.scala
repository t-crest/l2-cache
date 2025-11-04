package caches.hardware.util

import chisel3._

object PipelineReg {
  /** Returns a pipeline register, which can be enabled or disabled. The register accepts a default value too,
   *  to which it can be reset to.
   *
   * @example {{{
   * val pipeReg = PipelineReg(next, init, en, reset)
   * }}}
   */
  def apply[T <: Data](next: T, init: T, en: Bool, reset: Bool = false.B): T = {
    val pipelineReg = RegInit(init)

    when(reset) {
      pipelineReg := init
    } .elsewhen(en) {
      pipelineReg := next
    }

    pipelineReg
  }
}