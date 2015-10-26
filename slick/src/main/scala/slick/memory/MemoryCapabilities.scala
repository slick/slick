package slick.memory

import slick.basic.Capability

/** Capabilities for [[MemoryProfile]]. */
object MemoryCapabilities {
  /** Supports all MemoryProfile features which do not have separate capability values */
  val other = Capability("memory.other")

  /** All MemoryProfile capabilities */
  val all = Set(other)
}
