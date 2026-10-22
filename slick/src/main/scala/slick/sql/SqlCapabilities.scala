package slick.sql

import slick.basic.{Capabilities, Capability}
import slick.relational.RelationalSequenceComponent.SequenceParameter
import slick.relational.RelationalSequenceComponent.SequenceParameter.*

/** Capabilities for [[slick.sql.SqlProfile]]. */
object SqlCapabilities { caps =>
  /** Supports sequences (real or emulated) */
  val sequence = Capability("sql.sequence")
  /** Can get current sequence value */
  val sequenceCurr = Capability("sql.sequenceCurr")
  /** Supports limited sequences depending on min, max or cycle enabled */
  final class SequenceLimited[MIN <: SequenceParameter, MAX <: SequenceParameter, CYCLE <: SequenceParameter](min: MIN, max: MAX, cycle: CYCLE)
    extends Capability(s"sql.sequenceLimited.min.${min}.max.${max}.cycle.${cycle}") {
    type Min = MIN
    type Max = MAX
    type Cycle = CYCLE
  }

  /** Supports all SqlProfile features which do not have separate capability values */
  val other = Capability("sql.other")

  /** All SQL capabilities */
  val all = new All {}.capabilities
  trait All extends Capabilities {
    implicit val otherSqlCap: Registration[caps.other.type] = register(caps.other)
    implicit val sequenceCap: Registration[caps.sequence.type] = register(caps.sequence)
    implicit val sequenceCurrCap: Registration[caps.sequenceCurr.type] = register(caps.sequenceCurr)

    implicit val sequenceLimitedNoMinNoMaxNoCycleCap: Registration[SequenceLimited[No.type, No.type, No.type]] = register(new SequenceLimited(No, No, No))
    implicit val sequenceLimitedNoMinNoMaxCycleCap: Registration[SequenceLimited[No.type, No.type, Yes.type]] = register(new SequenceLimited(No, No, Yes))
    implicit val sequenceLimitedNoMinMaxNoCycleCap: Registration[SequenceLimited[No.type, Yes.type, No.type]] = register(new SequenceLimited(No, Yes, No))
    implicit val sequenceLimitedNoMinMaxCycleCap: Registration[SequenceLimited[No.type, Yes.type, Yes.type]] = register(new SequenceLimited(No, Yes, Yes))
    implicit val sequenceLimitedMinNoMaxNoCycleCap: Registration[SequenceLimited[Yes.type, No.type, No.type]] = register(new SequenceLimited(Yes, No, No))
    implicit val sequenceLimitedMinNoMaxCycleCap: Registration[SequenceLimited[Yes.type, No.type, Yes.type]] = register(new SequenceLimited(Yes, No, Yes))
    implicit val sequenceLimitedMinMaxNoCycleCap: Registration[SequenceLimited[Yes.type, Yes.type, No.type]] = register(new SequenceLimited(Yes, Yes, No))
    implicit val sequenceLimitedMinMaxCycleCap: Registration[SequenceLimited[Yes.type, Yes.type, Yes.type]] = register(new SequenceLimited(Yes, Yes, Yes))
  }
}
