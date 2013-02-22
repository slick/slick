package scala.slick.profile

import scala.slick.ast.{Symbol, SymbolNamer}

/**
 * Basic profile for SQL-based drivers.
 */
trait SqlProfile extends RelationalProfile { driver: SqlDriver =>

  override protected def computeCapabilities = super.computeCapabilities ++ SqlProfile.capabilities.all
}

object SqlProfile {
  object capabilities {
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
}

trait SqlDriver extends RelationalDriver with SqlProfile with SqlUtilsComponent {
  override val profile: SqlProfile = this
}

trait SqlUtilsComponent { driver: SqlDriver =>

  def quoteIdentifier(id: String): String = {
    val s = new StringBuilder(id.length + 4) append '"'
    for(c <- id) if(c == '"') s append "\"\"" else s append c
    (s append '"').toString
  }

  def likeEncode(s: String) = {
    val b = new StringBuilder
    for(c <- s) c match {
      case '%' | '_' | '^' => b append '^' append c
      case _ => b append c
    }
    b.toString
  }

  class QuotingSymbolNamer(parent: Option[SymbolNamer]) extends SymbolNamer("x", parent) {
    override def namedSymbolName(s: Symbol) = quoteIdentifier(s.name)
  }
}
