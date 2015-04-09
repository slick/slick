package slick.driver

import java.sql.{Timestamp, Time, Date}
import scala.concurrent.ExecutionContext
import slick.SlickException
import slick.dbio._
import slick.lifted._
import slick.ast._
import slick.util.MacroSupport.macroSupportInterpolation
import slick.profile.{RelationalProfile, SqlProfile, Capability}
import slick.compiler.CompilerState
import slick.model.Model
import slick.jdbc.{JdbcModelBuilder, JdbcType}
import slick.jdbc.meta.{MPrimaryKey, MColumn, MTable}

/** Slick driver for SQLite.
  *
  * This driver implements [[slick.driver.JdbcProfile]]
  * ''without'' the following capabilities:
  *
  * <ul>
  *   <li>[[slick.profile.RelationalProfile.capabilities.functionDatabase]],
  *     [[slick.profile.RelationalProfile.capabilities.functionUser]]:
  *     <code>Functions.user</code> and <code>Functions.database</code> are
  *     not available in SQLite. Slick will return empty strings for both.</li>
  *   <li>[[slick.profile.RelationalProfile.capabilities.joinFull]],
  *     [[slick.profile.RelationalProfile.capabilities.joinRight]]:
  *     Right and full outer joins are emulated because there is not native
  *     support for them.</li>
  *   <li>[[slick.driver.JdbcProfile.capabilities.mutable]]:
  *     SQLite does not allow mutation of result sets. All cursors are
  *     read-only.</li>
  *   <li>[[slick.profile.SqlProfile.capabilities.sequence]]:
  *     Sequences are not supported by SQLite.</li>
  *   <li>[[slick.driver.JdbcProfile.capabilities.returnInsertOther]]:
  *     When returning columns from an INSERT operation, only a single column
  *     may be specified which must be the table's AutoInc column.</li>
  *   <li>[[slick.profile.RelationalProfile.capabilities.typeBigDecimal]]:
  *     SQLite does not support a decimal type.</li>
  *   <li>[[slick.profile.RelationalProfile.capabilities.typeBlob]]: Blobs are
  *     not supported by the SQLite JDBC driver (but binary data in the form of
  *     <code>Array[Byte]</code> is).</li>
  *   <li>[[slick.profile.RelationalProfile.capabilities.zip]]:
  *     Row numbers (required by <code>zip</code> and
  *     <code>zipWithIndex</code>) are not supported. Trying to generate SQL
  *     code which uses this feature throws a SlickException.</li>
  *   <li>[[slick.driver.JdbcProfile.capabilities.insertOrUpdate]]:
  *     InsertOrUpdate operations are emulated on the client side if the
  *     data to insert contains an `AutoInc` field. Otherwise the operation
  *     is performmed natively on the server side.</li>
  *   <li>[[slick.driver.JdbcProfile.capabilities.defaultValueMetaData]]:
  *     The stable xerial sqlite-jdbc driver 3.7.2 does not return default values
  *     for columns in the DatabaseMetaData. Consequently they also do not appear
  *     in Slick's model. This has been fixed in sqlite-jdbc, but the only released
  *     version that contains the fix is milestone 3.7.15-M1. You can use it instead
  *     of the stable 3.7.2 in order to get default values with SQLite.
  *     Also see https://code.google.com/p/sqlite-jdbc/issues/detail?id=27
  *     </li>
  *   <li>[[slick.driver.JdbcProfile.capabilities.booleanMetaData]]:
  *     SQlite doesn't have booleans, so Slick maps to INTEGER instead.
  *     Other jdbc drivers like MySQL map TINYINT(1) back to a Scala
  *     Boolean. SQlite maps INTEGER to an Integer and that's how it shows
  *     up in the jdbc meta data, thus the original type is lost.</li>
  *   <li>[[slick.driver.JdbcProfile.capabilities.distinguishesIntTypes]]:
  *     SQLite does not distinguish integer types and maps them all to Int
  *     in the meta data.</li>
  *   <li>[[slick.driver.JdbcProfile.capabilities.supportsByte]]:
  *     SQLite does not distinguish integer types and maps them all to Int
  *     in the meta data.</li>
  * </ul>
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
    - JdbcProfile.capabilities.insertOrUpdate
    - JdbcProfile.capabilities.defaultValueMetaData
    - JdbcProfile.capabilities.booleanMetaData
    - JdbcProfile.capabilities.supportsByte
    - JdbcProfile.capabilities.distinguishesIntTypes
  )

  class ModelBuilder(mTables: Seq[MTable], ignoreInvalidDefaults: Boolean)(implicit ec: ExecutionContext) extends JdbcModelBuilder(mTables, ignoreInvalidDefaults) {
    override def createColumnBuilder(tableBuilder: TableBuilder, meta: MColumn): ColumnBuilder = new ColumnBuilder(tableBuilder, meta) {
      /** Regex matcher to extract name and length out of a db type name with length ascription */
      final val TypePattern = "^([A-Z]+)(\\(([0-9]+)\\))?$".r
      private val (_dbType,_size) = meta.typeName match {
        case TypePattern(d,_,s) => (d, Option(s).map(_.toInt))
      }
      override def dbType = Some(_dbType)
      override def length = _size
      override def varying = dbType == Some("VARCHAR")
      override def default = meta.columnDef.map((_,tpe)).collect{
        case ("null",_)  => Some(None) // 3.7.15-M1
      }.getOrElse{super.default}
      override def tpe = dbType match {
        case Some("DOUBLE") => "Double"
        case Some("DATE") => "java.sql.Date"
        case Some("TIME") => "java.sql.Time"
        case Some("TIMESTAMP") => "java.sql.Timestamp"
        case Some("BLOB") => "java.sql.Blob"
        case _ => super.tpe
      }
    }
    override def createPrimaryKeyBuilder(tableBuilder: TableBuilder, meta: Seq[MPrimaryKey]): PrimaryKeyBuilder = new PrimaryKeyBuilder(tableBuilder, meta) {
      // in 3.7.15-M1:
      override def columns = super.columns.map(_.stripPrefix("\"").stripSuffix("\""))
    }
  }

  override def createModelBuilder(tables: Seq[MTable], ignoreInvalidDefaults: Boolean)(implicit ec: ExecutionContext): JdbcModelBuilder =
    new ModelBuilder(tables, ignoreInvalidDefaults)

  override def defaultTables(implicit ec: ExecutionContext): DBIO[Seq[MTable]] =
    MTable.getTables(Some(""), Some(""), None, Some(Seq("TABLE")))
      .map(_.filter(_.name.name.toLowerCase != "sqlite_sequence"))

  override val columnTypes = new JdbcTypes
  override def createQueryBuilder(n: Node, state: CompilerState): QueryBuilder = new QueryBuilder(n, state)
  override def createUpsertBuilder(node: Insert): InsertBuilder = new UpsertBuilder(node)
  override def createTableDDLBuilder(table: Table[_]): TableDDLBuilder = new TableDDLBuilder(table)
  override def createColumnDDLBuilder(column: FieldSymbol, table: Table[_]): ColumnDDLBuilder = new ColumnDDLBuilder(column)
  override def createCountingInsertInvoker[U](compiled: CompiledInsert) = new CountingInsertInvoker[U](compiled)

  class QueryBuilder(tree: Node, state: CompilerState) extends super.QueryBuilder(tree, state) {
    override protected val supportsTuples = false
    override protected val concatOperator = Some("||")
    override protected val parenthesizeNestedRHSJoin = true

    override protected def buildOrdering(n: Node, o: Ordering) {
      if(o.nulls.last && !o.direction.desc)
        b"($n) is null,"
      else if(o.nulls.first && o.direction.desc)
        b"($n) is null desc,"
      expr(n)
      if(o.direction.desc) b" desc"
    }

    override protected def buildFetchOffsetClause(fetch: Option[Node], offset: Option[Node]) = (fetch, offset) match {
      case (Some(t), Some(d)) => b"\nlimit $d,$t"
      case (Some(t), None   ) => b"\nlimit $t"
      case (None,    Some(d)) => b"\nlimit $d,-1"
      case _ =>
    }

    override def expr(c: Node, skipParens: Boolean = false): Unit = c match {
      case Library.UCase(ch) => b"upper(!$ch)"
      case Library.LCase(ch) => b"lower(!$ch)"
      case Library.Substring(n, start, end) =>
        b"substr($n, ${QueryParameter.constOp[Int]("+")(_ + _)(start, LiteralNode(1))}, ${QueryParameter.constOp[Int]("-")(_ - _)(end, start)})"
      case Library.Substring(n, start) =>
        b"substr($n, ${QueryParameter.constOp[Int]("+")(_ + _)(start, LiteralNode(1))})\)"
      case Library.IndexOf(n, str) => b"\(charindex($str, $n) - 1\)"
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
      // https://github.com/jOOQ/jOOQ/issues/1595
      case Library.Repeat(n, times) => b"replace(substr(quote(zeroblob(($times + 1) / 2)), 3, $times), '0', $n)"
      case _ => super.expr(c, skipParens)
    }
  }

  /* Extending super.InsertBuilder here instead of super.UpsertBuilder. INSERT OR REPLACE is almost identical to INSERT. */
  class UpsertBuilder(ins: Insert) extends super.InsertBuilder(ins) {
    override protected def buildInsertStart = allNames.mkString(s"insert or replace into $tableName (", ",", ") ")
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
      else if(primaryKey) sb append " PRIMARY KEY"
      if(notNull) sb append " NOT NULL"
    }
  }

  class CountingInsertInvoker[U](compiled: CompiledInsert) extends super.CountingInsertInvoker[U](compiled) {
    // SQLite cannot perform server-side insert-or-update with soft insert semantics. We don't have to do
    // the same in ReturningInsertInvoker because SQLite does not allow returning non-AutoInc keys anyway.
    override protected val useServerSideUpsert = compiled.upsert.fields.forall(fs => !fs.options.contains(ColumnOption.AutoInc))
    override protected def useTransactionForUpsert = !useServerSideUpsert
  }

  override def defaultSqlTypeName(tmd: JdbcType[_], size: Option[RelationalProfile.ColumnOption.Length]): String = tmd.sqlType match {
    case java.sql.Types.TINYINT | java.sql.Types.SMALLINT | java.sql.Types.BIGINT => "INTEGER"
    case _ => super.defaultSqlTypeName(tmd, size)
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
      override def sqlTypeName(size: Option[RelationalProfile.ColumnOption.Length]) = "INTEGER"
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
