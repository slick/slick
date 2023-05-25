package slick.sql

import slick.basic.Capability

/** Capabilities for [[slick.sql.SqlProfile]]. */
object SqlCapabilities {
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
  val all = Set(other, sequence, sequenceCurr, sequenceCycle,
    sequenceLimited, sequenceMax, sequenceMin)
}
