package slick.memory

import slick.basic.{Capabilities, Capability}

/** Capabilities for [[MemoryProfile]]. */
object MemoryCapabilities { caps =>
  /** Supports all MemoryProfile features which do not have separate capability values */
  val other = Capability("memory.other")

  /** All MemoryProfile capabilities */
  val all = new All {}.capabilities
  trait All extends Capabilities {
    implicit val otherMemoryCap: Registration[caps.other.type] = register(caps.other)
  }
}
