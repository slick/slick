package scala.slick.profile

import scala.slick.ast.{Symbol, SymbolNamer}

/**
 * Basic profile for SQL-based drivers.
 */
trait SqlProfile extends BasicProfile { driver: SqlDriver =>

  override protected def computeCapabilities = super.computeCapabilities ++ SqlProfile.capabilities.all
}

object SqlProfile {
  object capabilities {
    /** Supports default values in column definitions */
    val columnDefaults = Capability("sql.columnDefaults")
    /** Supports foreignKeyActions */
    val foreignKeyActions = Capability("sql.foreignKeyActions")
    /** Supports the ''database'' function to get the current database name.
      * A driver without this capability will return an empty string. */
    val functionDatabase = Capability("sql.functionDatabase")
    /** Supports the ''user'' function to get the current database user.
      * A driver without this capability will return an empty string. */
    val functionUser = Capability("sql.functionUser")
    /** Supports full outer joins */
    val joinFull = Capability("sql.joinFull")
    /** Supports right outer joins */
    val joinRight = Capability("sql.joinRight")
    /** Supports escape characters in "like" */
    val likeEscape = Capability("sql.likeEscape")
    /** Supports .drop on queries */
    val pagingDrop = Capability("sql.pagingDrop")
    /** Supports properly compositional paging in sub-queries */
    val pagingNested = Capability("sql.pagingNested")
    /** Returns only the requested number of rows even if some rows are not
      * unique. Without this capability, non-unique rows may be counted as
      * only one row each. */
    val pagingPreciseTake = Capability("sql.pagingPreciseTake")
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
    /** Can set an Option[ Array[Byte] ] column to None */
    val setByteArrayNull = Capability("sql.setByteArrayNull")
    /** Supports the BigDecimal data type */
    val typeBigDecimal = Capability("sql.typeBigDecimal")
    /** Supports the Blob data type */
    val typeBlob = Capability("sql.typeBlob")
    /** Supports the Long data type */
    val typeLong = Capability("sql.typeLong")
    /** Supports zip, zipWith and zipWithIndex */
    val zip = Capability("sql.zip")

    /** Supports all JdbcProfile features which do not have separate capability values */
    val other = Capability("sql.other")

    /** All SQL capabilities */
    val all = Set(other, columnDefaults, foreignKeyActions, functionDatabase,
      functionUser, joinFull, joinRight, likeEscape, pagingDrop, pagingNested,
      pagingPreciseTake, sequence, sequenceCurr, sequenceCycle,
      sequenceLimited, sequenceMax, sequenceMin, setByteArrayNull,
      typeBigDecimal, typeBlob, typeLong, zip)
  }
}

trait SqlDriver extends BasicDriver with SqlProfile with SqlUtilsComponent

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
