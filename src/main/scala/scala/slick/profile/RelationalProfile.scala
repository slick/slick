package scala.slick.profile

import scala.slick.ast.{CollectionTypeConstructor, CollectionType, Node, Select, TypedType, ColumnOption, Ref, FieldSymbol}
import scala.slick.lifted.{Column, AbstractTable}

/**
 * A profile for relational databases that does not assume the existence
 * of SQL (or some other text-based language for executing statements).
 */
trait RelationalProfile extends BasicProfile with RelationalTableComponent { driver: RelationalDriver =>

  override protected def computeCapabilities = super.computeCapabilities ++ RelationalProfile.capabilities.all
}

object RelationalProfile {
  object capabilities {
    /** Supports default values in column definitions */
    val columnDefaults = Capability("relational.columnDefaults")
    /** Supports foreignKeyActions */
    val foreignKeyActions = Capability("relational.foreignKeyActions")
    /** Supports the ''database'' function to get the current database name.
      * A driver without this capability will return an empty string. */
    val functionDatabase = Capability("relational.functionDatabase")
    /** Supports the ''user'' function to get the current database user.
      * A driver without this capability will return an empty string. */
    val functionUser = Capability("relational.functionUser")
    /** Supports full outer joins */
    val joinFull = Capability("relational.joinFull")
    /** Supports right outer joins */
    val joinRight = Capability("relational.joinRight")
    /** Supports escape characters in "like" */
    val likeEscape = Capability("relational.likeEscape")
    /** Supports .drop on queries */
    val pagingDrop = Capability("relational.pagingDrop")
    /** Supports properly compositional paging in sub-queries */
    val pagingNested = Capability("relational.pagingNested")
    /** Returns only the requested number of rows even if some rows are not
      * unique. Without this capability, non-unique rows may be counted as
      * only one row each. */
    val pagingPreciseTake = Capability("relational.pagingPreciseTake")
    /** Can set an Option[ Array[Byte] ] column to None */
    val setByteArrayNull = Capability("relational.setByteArrayNull")
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
    val all = Set(other, columnDefaults, foreignKeyActions, functionDatabase,
      functionUser, joinFull, joinRight, likeEscape, pagingDrop, pagingNested,
      pagingPreciseTake, setByteArrayNull, typeBigDecimal, typeBlob, typeLong,
      zip)
  }
}

trait RelationalDriver extends BasicDriver with RelationalProfile

trait RelationalTableComponent { driver: RelationalDriver =>

  trait ColumnOptions {
    val PrimaryKey = ColumnOption.PrimaryKey
    def Default[T](defaultValue: T) = ColumnOption.Default[T](defaultValue)
    val AutoInc = ColumnOption.AutoInc
  }

  val columnOptions: ColumnOptions = new AnyRef with ColumnOptions

  abstract class Table[T](_schemaName: Option[String], _tableName: String) extends AbstractTable[T](_schemaName, _tableName) { table =>
    def this(_tableName: String) = this(None, _tableName)

    val O: driver.columnOptions.type = columnOptions

    def column[C](n: String, options: ColumnOption[C]*)(implicit tm: TypedType[C]): Column[C] = new Column[C] {
      override def nodeDelegate =
        Select(Node(table) match {
          case r: Ref => r
          case _ => Ref(Node(table).nodeIntrinsicSymbol)
        }, FieldSymbol(n)(options, tm)).nodeTyped(tm)
      override def toString = (Node(table) match {
        case r: Ref => "(" + _tableName + " " + r.sym.name + ")"
        case _ => _tableName
      }) + "." + n
    }

    def tpe = CollectionType(CollectionTypeConstructor.default, *.tpe)
  }
}
