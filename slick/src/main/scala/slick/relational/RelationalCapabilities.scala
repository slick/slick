package slick.relational

import slick.basic.{Capabilities, Capability}

/** Capabilities for [[slick.relational.RelationalProfile]]. */
object RelationalCapabilities { caps =>
  /** Computes `lengthLong` and `countDistinctLong` with a 64-bit count.
    * A profile without this capability uses a 32-bit count, so these
    * methods are limited to the `Int` range. */
  val countLong = Capability("relational.countLong")
  /** Supports default values in column definitions */
  val columnDefaults = Capability("relational.columnDefaults")
  /** Supports foreignKeyActions */
  val foreignKeyActions = Capability("relational.foreignKeyActions") // todo:caps
  /** Supports the ''database'' function to get the current database name.
    * A profile without this capability will return an empty string. */
  val functionDatabase = Capability("relational.functionDatabase")
  /** Supports the ''user'' function to get the current database user.
    * A profile without this capability will return an empty string. */
  val functionUser = Capability("relational.functionUser")
  /** Supports indexOf method on string columns */
  val indexOf = Capability("relational.indexOf")
  /** Supports repeat method on string columns */
  val repeat = Capability("relational.repeat")
  /** Supports full outer joins */
  val joinFull = Capability("relational.joinFull")
  /** Supports left outer joins */
  val joinLeft = Capability("relational.joinLeft") // todo:caps-unused
  /** Supports right outer joins */
  val joinRight = Capability("relational.joinRight")
  /** Supports escape characters in "like" */
  val likeEscape = Capability("relational.likeEscape")
  /** Supports .drop on queries */
  val pagingDrop = Capability("relational.pagingDrop")
  /** Supports properly compositional paging in sub-queries */
  val pagingNested = Capability("relational.pagingNested") // todo:caps-unused
  /** Returns only the requested number of rows even if some rows are not
    * unique. Without this capability, non-unique rows may be counted as
    * only one row each. */
  val pagingPreciseTake = Capability("relational.pagingPreciseTake") // todo:caps-unused
  /** Supports replace method on string columns */
  val replace = Capability("relational.replace")
  /** Supports reverse method on string columns */
  val reverse = Capability("relational.reverse")
  /** Can set an Option[ Array[Byte] ] column to None */
  val setByteArrayNull = Capability("relational.setByteArrayNull") // todo:caps-unused
  /** Supports the BigDecimal data type */
  val typeBigDecimal = Capability("relational.typeBigDecimal")
  /** Supports the Blob data type */
  val typeBlob = Capability("relational.typeBlob")
  /** Supports the Long data type */
  val typeLong = Capability("relational.typeLong")
  /** Supports zip, zipWith and zipWithIndex */
  val zip = Capability("relational.zip")

  /** Supports all RelationalProfile features which do not have separate capability values */
  val other = Capability("relational.other")

  /** All relational capabilities */
  val all = new All{}.capabilities
  trait All extends Capabilities {
    implicit val otherRelationalCap: Registration[caps.other.type] = register(caps.other)
    implicit val columnDefaultsCap: Registration[caps.columnDefaults.type] = register(caps.columnDefaults)
    implicit val countLongCap: Registration[caps.countLong.type] = register(caps.countLong)
    implicit val foreignKeyActionsCap: Registration[caps.foreignKeyActions.type] = register(caps.foreignKeyActions)
    implicit val functionDatabaseCap: Registration[caps.functionDatabase.type] = register(caps.functionDatabase)
    implicit val functionUserCap: Registration[caps.functionUser.type] = register(caps.functionUser)
    implicit val joinFullCap: Registration[caps.joinFull.type] = register(caps.joinFull)
    implicit val joinLeftCap: Registration[caps.joinLeft.type] = register(caps.joinLeft)
    implicit val joinRightCap: Registration[caps.joinRight.type] = register(caps.joinRight)
    implicit val likeEscapeCap: Registration[caps.likeEscape.type] = register(caps.likeEscape)
    implicit val pagingDropCap: Registration[caps.pagingDrop.type] = register(caps.pagingDrop)
    implicit val pagingNestedCap: Registration[caps.pagingNested.type] = register(caps.pagingNested)
    implicit val pagingPreciseTakeCap: Registration[caps.pagingPreciseTake.type] = register(caps.pagingPreciseTake)
    implicit val setByteArrayNullCap: Registration[caps.setByteArrayNull.type] = register(caps.setByteArrayNull)
    implicit val typeBigDecimalCap: Registration[caps.typeBigDecimal.type] = register(caps.typeBigDecimal)
    implicit val typeBlobCap: Registration[caps.typeBlob.type] = register(caps.typeBlob)
    implicit val typeLongCap: Registration[caps.typeLong.type] = register(caps.typeLong)
    implicit val zipCap: Registration[caps.zip.type] = register(caps.zip)
    implicit val replaceCap: Registration[caps.replace.type] = register(caps.replace)
    implicit val reverseCap: Registration[caps.reverse.type] = register(caps.reverse)
    implicit val indexOfCap: Registration[caps.indexOf.type] = register(caps.indexOf)
    implicit val repeatCap: Registration[caps.repeat.type] = register(caps.repeat)
  }
}
