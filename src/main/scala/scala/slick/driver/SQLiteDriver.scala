package scala.slick.driver

import scala.slick.SlickException
import scala.slick.lifted._
import scala.slick.ast._
import scala.slick.util.MacroSupport.macroSupportInterpolation
import java.sql.{Timestamp, Time, Date}
import scala.slick.profile.{RelationalProfile, SqlProfile, Capability}
import scala.slick.compiler.CompilerState

/**
 * Slick driver for SQLite.
 *
 * This driver implements the [[scala.slick.driver.ExtendedProfile]]
 * ''without'' the following capabilities:
 *
 * <ul>
 *   <li>[[scala.slick.profile.RelationalProfile.capabilities.functionDatabase]],
 *     [[scala.slick.profile.RelationalProfile.capabilities.functionUser]]:
 *     <code>Functions.user</code> and <code>Functions.database</code> are
 *     not available in SQLite. Slick will return empty strings for both.</li>
 *   <li>[[scala.slick.profile.RelationalProfile.capabilities.joinFull]],
 *     [[scala.slick.profile.RelationalProfile.capabilities.joinRight]]:
 *     Right and full outer joins are not supported by SQLite.</li>
 *   <li>[[scala.slick.driver.JdbcProfile.capabilities.mutable]]:
 *     SQLite does not allow mutation of result sets. All cursors are
 *     read-only.</li>
 *   <li>[[scala.slick.profile.SqlProfile.capabilities.sequence]]:
 *     Sequences are not supported by SQLite.</li>
 *   <li>[[scala.slick.driver.JdbcProfile.capabilities.returnInsertOther]]:
 *     When returning columns from an INSERT operation, only a single column
 *     may be specified which must be the table's AutoInc column.</li>
 *   <li>[[scala.slick.profile.RelationalProfile.capabilities.typeBigDecimal]]:
 *     SQLite does not support a decimal type.</li>
 *   <li>[[scala.slick.profile.RelationalProfile.capabilities.typeBlob]]: Blobs are
 *     not supported by the SQLite JDBC driver (but binary data in the form of
 *     <code>Array[Byte]</code> is).</li>
 *   <li>[[scala.slick.profile.RelationalProfile.capabilities.zip]]:
 *     Row numbers (required by <code>zip</code> and
 *     <code>zipWithIndex</code>) are not supported. Trying to generate SQL
 *     code which uses this feature throws a SlickException.</li>
 * </ul>
 *
 * @author Paul Snively
 * @author Stefan Zeiger
 */
trait SQLiteDriver extends JdbcDriver { driver =>

  override protected def computeCapabilities: Set[Capability] = (super.computeCapabilities
    - RelationalProfile.capabilities.functionDatabase
    - RelationalProfile.capabilities.functionUser
    - RelationalProfile.capabilities.joinFull
    - RelationalProfile.capabilities.joinRight
    - JdbcProfile.capabilities.mutable
    - SqlProfile.capabilities.sequence
    - JdbcProfile.capabilities.returnInsertOther
    - RelationalProfile.capabilities.typeBigDecimal
    - RelationalProfile.capabilities.typeBlob
    - RelationalProfile.capabilities.zip
  )

  override val columnTypes = new JdbcTypes
  override def createQueryBuilder(n: Node, state: CompilerState): QueryBuilder = new QueryBuilder(n, state)
  override def createTableDDLBuilder(table: Table[_]): TableDDLBuilder = new TableDDLBuilder(table)
  override def createColumnDDLBuilder(column: FieldSymbol, table: Table[_]): ColumnDDLBuilder = new ColumnDDLBuilder(column)

  class QueryBuilder(tree: Node, state: CompilerState) extends super.QueryBuilder(tree, state) {
    override protected val supportsTuples = false
    override protected val concatOperator = Some("||")

    override protected def buildOrdering(n: Node, o: Ordering) {
      if(o.nulls.last && !o.direction.desc)
        b"($n) is null,"
      else if(o.nulls.first && o.direction.desc)
        b"($n) is null desc,"
      expr(n)
      if(o.direction.desc) b" desc"
    }

    override protected def buildFetchOffsetClause(fetch: Option[Long], offset: Option[Long]) = (fetch, offset) match {
      case (Some(t), Some(d)) => b" LIMIT $d,$t"
      case (Some(t), None   ) => b" LIMIT $t"
      case (None,    Some(d)) => b" LIMIT $d,-1"
      case _ =>
    }

    override def expr(c: Node, skipParens: Boolean = false): Unit = c match {
      case Library.UCase(ch) => b"upper(!$ch)"
      case Library.LCase(ch) => b"lower(!$ch)"
      case Library.%(l, r) => b"\($l%$r\)"
      case Library.Ceiling(ch) => b"round($ch+0.5)"
      case Library.Floor(ch) => b"round($ch-0.5)"
      case Library.User() => b"''"
      case Library.Database() => b"''"
      case Apply(j: Library.JdbcFunction, ch) if j != Library.Concat =>
        /* The SQLite JDBC driver does not support ODBC {fn ...} escapes, so we try
         * unescaped function calls by default */
        b"${j.name}("
        b.sep(ch, ",")(expr(_, true))
        b")"
      case s: SimpleFunction if s.scalar =>
        /* The SQLite JDBC driver does not support ODBC {fn ...} escapes, so we try
         * unescaped function calls by default */
        b"${s.name}("
        b.sep(s.nodeChildren, ",")(expr(_, true))
        b")"
      case RowNumber(_) => throw new SlickException("SQLite does not support row numbers")
      case _ => super.expr(c, skipParens)
    }
  }

  class TableDDLBuilder(table: Table[_]) extends super.TableDDLBuilder(table) {
    override protected val foreignKeys = Nil // handled directly in addTableOptions
    override protected val primaryKeys = Nil // handled directly in addTableOptions

    override protected def addTableOptions(b: StringBuilder) {
      for(pk <- table.primaryKeys) {
        b append ","
        addPrimaryKey(pk, b)
      }
      for(fk <- table.foreignKeys) {
        b append ","
        addForeignKey(fk, b)
      }
    }
  }

  class ColumnDDLBuilder(column: FieldSymbol) extends super.ColumnDDLBuilder(column) {
    override protected def appendOptions(sb: StringBuilder) {
      if(defaultLiteral ne null) sb append " DEFAULT " append defaultLiteral
      if(autoIncrement) sb append " PRIMARY KEY AUTOINCREMENT"
      else if(notNull) sb append " NOT NULL"
      else if(primaryKey) sb append " PRIMARY KEY"
    }
  }

  class JdbcTypes extends super.JdbcTypes {
    override val booleanJdbcType = new BooleanJdbcType
    override val dateJdbcType = new DateJdbcType
    override val timeJdbcType = new TimeJdbcType
    override val timestampJdbcType = new TimestampJdbcType
    override val uuidJdbcType = new UUIDJdbcType

    /* SQLite does not have a proper BOOLEAN type. The suggested workaround is
     * INTEGER with constants 1 and 0 for TRUE and FALSE. */
    class BooleanJdbcType extends super.BooleanJdbcType {
      override def sqlTypeName = "INTEGER"
      override def valueToSQLLiteral(value: Boolean) = if(value) "1" else "0"
    }
    /* The SQLite JDBC driver does not support the JDBC escape syntax for
     * date/time/timestamp literals. SQLite expects these values as milliseconds
     * since epoch. */
    class DateJdbcType extends super.DateJdbcType {
      override def valueToSQLLiteral(value: Date) = value.getTime.toString
    }
    class TimeJdbcType extends super.TimeJdbcType {
      override def valueToSQLLiteral(value: Time) = value.getTime.toString
    }
    class TimestampJdbcType extends super.TimestampJdbcType {
      override def valueToSQLLiteral(value: Timestamp) = value.getTime.toString
    }
    class UUIDJdbcType extends super.UUIDJdbcType {
      override def sqlType = java.sql.Types.BLOB
    }
  }
}

object SQLiteDriver extends SQLiteDriver
