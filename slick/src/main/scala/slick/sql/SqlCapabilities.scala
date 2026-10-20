package slick.sql

import slick.basic.{Capabilities, Capability}

/** Capabilities for [[slick.sql.SqlProfile]]. */
object SqlCapabilities { caps =>
  /** Supports sequences (real or emulated) */
  val sequence = Capability("sql.sequence")
  /** Can get current sequence value */
  val sequenceCurr = Capability("sql.sequenceCurr")
  /** Supports cyclic sequences */
  val sequenceCycle = Capability("sql.sequenceCycle")
  /** Supports non-cyclic limited sequences (with a max value) */
  val sequenceLimited = Capability("sql.sequenceLimited")
  /** Supports max value for sequences */
  val sequenceMax = Capability("sql.sequenceMax")
  /** Supports min value for sequences */
  val sequenceMin = Capability("sql.sequenceMin")

  /** Supports all SqlProfile features which do not have separate capability values */
  val other = Capability("sql.other")

  /** All SQL capabilities */
  val all = new All{}.capabilities
  trait All extends Capabilities {
    implicit val otherSqlCap: Registration[caps.other.type] = register(caps.other)
    implicit val sequenceCap: Registration[caps.sequence.type] = register(caps.sequence)
    implicit val sequenceCurrCap: Registration[caps.sequenceCurr.type] = register(caps.sequenceCurr)
    implicit val sequenceCycleCap: Registration[caps.sequenceCycle.type] = register(caps.sequenceCycle)
    implicit val sequenceLimitedCap: Registration[caps.sequenceLimited.type] = register(caps.sequenceLimited)
    implicit val sequenceMaxCap: Registration[caps.sequenceMax.type] = register(caps.sequenceMax)
    implicit val sequenceMinCap: Registration[caps.sequenceMin.type] = register(caps.sequenceMin)
  }
}
