package slick

/** Contains the abstract `MemoryQueryingProfile` and related code, as well as the concrete
  * `MemoryProfile` and `DistributedProfile` implementations for in-memory interpretation of
  * queries and scheduling of distributed queries (i.e. combining several profiles and backends). */
package object memory {

  @deprecated("Use `slick.memory.MemoryProfile` instead of `slick.memory.MemoryDriver`", "3.2")
  type MemoryDriver = MemoryProfile
  @deprecated("Use `slick.memory.MemoryProfile` instead of `slick.memory.MemoryDriver`", "3.2")
  val MemoryDriver = MemoryProfile

  @deprecated("Use `slick.memory.MemoryQueryingProfile` instead of `slick.memory.MemoryQueryingDriver`", "3.2")
  type MemoryQueryingDriver = MemoryQueryingProfile

  @deprecated("Use `slick.memory.DistributedProfile` instead of `slick.memory.DistributedDriver`", "3.2")
  type DistributedDriver = DistributedProfile
}
