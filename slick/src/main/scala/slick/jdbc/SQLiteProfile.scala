package slick.jdbc

import java.sql.{Date, Time, Timestamp}
import java.time.*
import java.time.format.DateTimeFormatter
import java.util.UUID

import scala.concurrent.ExecutionContext

import slick.relational.RelationalCapabilities
import slick.sql.SqlCapabilities
import slick.SlickException
import slick.ast.*
import slick.basic.Capability
import slick.compiler.CompilerState
import slick.dbio.*
import slick.jdbc.meta.{MColumn, MPrimaryKey, MTable}
import slick.util.QueryInterpolator.queryInterpolator

/** Slick profile for SQLite.
  *
  * This profile implements [[slick.jdbc.JdbcProfile]]
  * ''without'' the following capabilities:
  *
  * <ul>
  *   <li>[[slick.relational.RelationalCapabilities.functionDatabase]],
  *     [[slick.relational.RelationalCapabilities.functionUser]]:
  *     <code>Functions.user</code> and <code>Functions.database</code> are
  *     not available in SQLite. Slick will return empty strings for both.</li>
  *   <li>[[slick.relational.RelationalCapabilities.joinFull]],
  *     [[slick.relational.RelationalCapabilities.joinRight]]:
  *     Right and full outer joins are emulated because there is not native
  *     support for them.</li>
  *   <li>[[slick.jdbc.JdbcCapabilities.mutable]]:
  *     SQLite does not allow mutation of result sets. All cursors are
  *     read-only.</li>
  *   <li>[[slick.sql.SqlCapabilities.sequence]]:
  *     Sequences are not supported by SQLite.</li>
  *   <li>[[slick.jdbc.JdbcCapabilities.returnInsertOther]]:
  *     When returning columns from an INSERT operation, only a single column
  *     may be specified which must be the table's AutoInc column.</li>
  *   <li>[[slick.relational.RelationalCapabilities.typeBigDecimal]]:
  *     SQLite does not support a decimal type.</li>
  *   <li>[[slick.relational.RelationalCapabilities.typeBlob]]: Blobs are
  *     not supported by the SQLite JDBC driver (but binary data in the form of
  *     <code>Array[Byte]</code> is).</li>
  *   <li>[[slick.relational.RelationalCapabilities.zip]]:
  *     Row numbers (required by <code>zip</code> and
  *     <code>zipWithIndex</code>) are not supported. Trying to generate SQL
  *     code which uses this feature throws a SlickException.</li>
  *   <li>[[slick.jdbc.JdbcCapabilities.insertOrUpdate]]:
  *     InsertOrUpdate operations are emulated on the client side if the
  *     data to insert contains an `AutoInc` field. Otherwise the operation
  *     is performed natively on the server side.</li>
  *   <li>[[slick.jdbc.JdbcCapabilities.defaultValueMetaData]]:
  *     The stable xerial sqlite-jdbc driver 3.7.2 does not return default values
  *     for columns in the DatabaseMetaData. Consequently they also do not appear
  *     in Slick's model. This has been fixed in sqlite-jdbc, but the only released
  *     version that contains the fix is milestone 3.7.15-M1. You can use it instead
  *     of the stable 3.7.2 in order to get default values with SQLite.
  *     Also see https://code.google.com/p/sqlite-jdbc/issues/detail?id=27
  *     </li>
  *   <li>[[slick.jdbc.JdbcCapabilities.booleanMetaData]]:
  *     SQLite doesn't have booleans, so Slick maps to INTEGER instead.
  *     Other jdbc drivers like MySQL map TINYINT(1) back to a Scala
  *     Boolean. SQLite maps INTEGER to an Integer and that's how it shows
  *     up in the jdbc meta data, thus the original type is lost.</li>
  *   <li>[[slick.jdbc.JdbcCapabilities.distinguishesIntTypes]]:
  *     SQLite does not distinguish integer types and maps them all to Int
  *     in the meta data.</li>
  *   <li>[[slick.jdbc.JdbcCapabilities.supportsByte]]:
  *     SQLite does not distinguish integer types and maps them all to Int
  *     in the meta data.</li>
  *   <li>[[slick.jdbc.JdbcCapabilities.returnMultipleInsertKey]]:
  *     SQLite returns the last generated key only.</li>
  * </ul>
  */
trait SQLiteProfile extends JdbcProfile with JdbcActionComponent.MultipleRowsPerStatementSupport {
  override protected def computeCapabilities: Set[Capability] =
    super.computeCapabilities -
      RelationalCapabilities.functionDatabase -
      RelationalCapabilities.functionUser -
      RelationalCapabilities.joinFull -
      RelationalCapabilities.joinRight -
      JdbcCapabilities.mutable -
      SqlCapabilities.sequence -
      JdbcCapabilities.returnInsertOther -
      RelationalCapabilities.typeBigDecimal -
      RelationalCapabilities.typeBlob -
      RelationalCapabilities.zip -
      JdbcCapabilities.insertOrUpdate -
      JdbcCapabilities.defaultValueMetaData -
      JdbcCapabilities.booleanMetaData -
      JdbcCapabilities.supportsByte -
      JdbcCapabilities.distinguishesIntTypes -
      JdbcCapabilities.forUpdate -
      JdbcCapabilities.returnMultipleInsertKey

  class SQLiteModelBuilder(mTables: Seq[MTable], ignoreInvalidDefaults: Boolean)(implicit ec: ExecutionContext)
    extends JdbcModelBuilder(mTables, ignoreInvalidDefaults) {

    override def createColumnBuilder(tableBuilder: TableBuilder, meta: MColumn): ColumnBuilder =
      new SQLiteColumnBuilder(tableBuilder, meta)
    override def createPrimaryKeyBuilder(tableBuilder: TableBuilder, meta: Seq[MPrimaryKey]): PrimaryKeyBuilder =
      new SQLitePrimaryKeyBuilder(tableBuilder, meta)

    class SQLiteColumnBuilder(tableBuilder: TableBuilder, meta: MColumn) extends ColumnBuilder(tableBuilder, meta) {

      // Regex matcher to extract name and length out of a db type name with length ascription
      final val TypePattern = "^([A-Z\\s]+)(?:\\(\\s*([0-9]+)\\s*,?\\s*(?:[0-9]+)?\\s*\\))?$".r

      def extractTypeProps(typeName: String): (String, Option[Int]) =
        typeName match {
          case TypePattern(name, length) => (name, Option(length).map(_.toInt))
          case "" => ("TEXT", None)
        }

      private val (extractedType, extractedLength) = extractTypeProps(meta.typeName)

      override def dbType: Some[String] = Some(extractedType)
      override def length = extractedLength
      override def varying = dbType.contains("VARCHAR")
      override def default: Option[Option[Any]] = meta.columnDef.map((_,tpe)).collect{
        case ("null",_)  => Some(None) // 3.7.15-M1
        case (v, "java.sql.Timestamp") =>
              import scala.util.{Success, Try}
              val convertors = Seq((s: String) => new java.sql.Timestamp(s.toLong),
                (s: String) => Timestamp.valueOf(s),
                (s: String) => Timestamp.from(Instant.from(DateTimeFormatter.ISO_DATE_TIME.parse(s))),
                (s: String) => Timestamp.from(Instant.from(DateTimeFormatter.ISO_DATE_TIME.parse(s.replace(' ', 'T')))),
                (s: String) =>
                  Timestamp.from(
                    LocalDate.from(DateTimeFormatter.ISO_LOCAL_DATE.parse(s)).atStartOfDay().toInstant(ZoneOffset.UTC)
                  ),
                (s: String) =>
                  Timestamp.from(
                    LocalTime.from(DateTimeFormatter.ISO_LOCAL_TIME.parse(s))
                      .atDate(LocalDate.ofEpochDay(0)).toInstant(ZoneOffset.UTC)
                  ),
                (s: String) => {
                  if (s == "now")
                    "new java.sql.Timestamp(java.util.Calendar.getInstance().getTime().getTime())"
                  else
                    throw new Exception(s"Failed to parse timestamp - $s")
                }
              )
              val v2 = v.replace("\"", "")
              convertors.iterator.map(fn => Try(fn(v2))).collectFirst {
            case Success(v) => Some(v)
          }
          }
          .getOrElse(super.default)
      override def tpe = dbType match {
        case Some("DOUBLE") => "Double"
        case Some("DATE") => "java.sql.Date"
        case Some("TIME") => "java.sql.Time"
        case Some("TIMESTAMP") => "java.sql.Timestamp"
        case Some("BLOB") => "java.sql.Blob"
        case _ => super.tpe
      }
    }

    class SQLitePrimaryKeyBuilder(tableBuilder: TableBuilder, meta: Seq[MPrimaryKey])
      extends PrimaryKeyBuilder(tableBuilder, meta) {
      // in 3.7.15-M1:
      override def columns = super.columns.map(_.stripPrefix("\"").stripSuffix("\""))
    }

    override def readIndices(t: MTable) = super.readIndices(t).map(
      _.filterNot(
        _.exists( _.indexName.exists(_.startsWith("sqlite_autoindex_")) )
      )
    )
  }

  override def createModelBuilder(tables: Seq[MTable], ignoreInvalidDefaults: Boolean)
                                 (implicit ec: ExecutionContext): JdbcModelBuilder =
    new SQLiteModelBuilder(tables, ignoreInvalidDefaults)

  override def defaultTables(implicit ec: ExecutionContext): DBIO[Seq[MTable]] =
    MTable.getTables(Some(""), Some(""), None, Some(Seq("TABLE")))
      .map(_.filter(_.name.name.toLowerCase != "sqlite_sequence"))

  override val columnTypes: SQLiteJdbcTypes = new SQLiteJdbcTypes
  override def createQueryBuilder(n: Node, state: CompilerState): QueryBuilder = new SQLiteQueryBuilder(n, state)
  override def createUpsertBuilder(node: Insert): super.InsertBuilder = new SQLiteUpsertBuilder(node)
  override def createInsertBuilder(node: Insert): super.InsertBuilder = new SQLiteInsertBuilder(node)
  override def createTableDDLBuilder(table: Table[?]): TableDDLBuilder = new SQLiteTableDDLBuilder(table)
  override def createColumnDDLBuilder(column: FieldSymbol, table: Table[?]): ColumnDDLBuilder =
    new SQLiteColumnDDLBuilder(column)

  private trait SQLiteInsertAll[U] extends InsertActionComposerImpl[U] {
    override def insertAll(values: Iterable[U], rowsPerStatement: RowsPerStatement = RowsPerStatement.All) =
      super.insertAll(
        values = values,
        rowsPerStatement =
          if (compiled.standardInsert.ibr.fields.isEmpty) {
            // Use batch insert because sqlite doesn't support multi default values
            RowsPerStatement.One
          } else
            rowsPerStatement
      )
  }
  override def createInsertActionExtensionMethods[T](compiled: CompiledInsert): InsertActionExtensionMethods[T] =
    new SQLiteCountingInsertActionComposerImpl[T](compiled) with SQLiteInsertAll[T]

  override def createReturningInsertActionComposer[U, QR, RU](compiled: JdbcCompiledInsert,
                                                              keys: Node,
                                                              mux: (U, QR) => RU
                                                             ): ReturningInsertActionComposer[U, RU] =
    new ReturningInsertActionComposerImpl[U, QR, RU](compiled, keys, mux) with SQLiteInsertAll[U]

  class SQLiteQueryBuilder(tree: Node, state: CompilerState) extends QueryBuilder(tree, state) {
    override protected val supportsTuples = false
    override protected val concatOperator: Some[String] = Some("||")
    override protected val parenthesizeNestedRHSJoin = true
    override protected val alwaysAliasSubqueries = false
    override protected val quotedJdbcFns: Some[Nil.type] = Some(Nil)

    override protected def buildOrdering(n: Node, o: Ordering): Unit = {
      if(o.nulls.last && !o.direction.desc)
        b"($n) is null,"
      else if(o.nulls.first && o.direction.desc)
        b"($n) is null desc,"
      expr(n, false)
      if(o.direction.desc) b" desc"
    }

    override protected def buildFetchOffsetClause(fetch: Option[Node], offset: Option[Node]) = (fetch, offset) match {
      case (Some(t), Some(d)) => b"\nlimit $d,$t"
      case (Some(t), None   ) => b"\nlimit $t"
      case (None,    Some(d)) => b"\nlimit $d,-1"
      case _ =>
    }

    override def expr(c: Node): Unit = c match {
      case Library.UCase(ch)                => b"upper(!$ch)"
      case Library.LCase(ch)                => b"lower(!$ch)"
      case Library.Substring(n, start, end) =>
        val startNode = QueryParameter.constOp[Int]("+")(_ + _)(start, LiteralNode(1).infer())
        val lengthNode = QueryParameter.constOp[Int]("-")(_ - _)(end, start)
        b"substr($n, $startNode, $lengthNode)"
      case Library.Substring(n, start)      =>
        b"substr($n, ${QueryParameter.constOp[Int]("+")(_ + _)(start, LiteralNode(1).infer())})\)"
      case Library.IndexOf(n, str)          => b"\(charindex($str, $n) - 1\)"
      case Library.%(l, r)                  => b"\($l%$r\)"
      case Library.Ceiling(ch)              => b"round($ch+0.5)"
      case Library.Floor(ch)                => b"round($ch-0.5)"
      case Library.User()                   => b"''"
      case Library.Database()               => b"''"
      case RowNumber(_)                     => throw new SlickException("SQLite does not support row numbers")
      // https://github.com/jOOQ/jOOQ/issues/1595
      case Library.Repeat(n, times) =>
        b"replace(substr(quote(zeroblob(($times + 1) / 2)), 3, $times), '0', $n)"
      case Union(left, right, all)  =>
        b"\{ select * from "
        b"\["
        buildFrom(left, None, skipParens = true)
        b"\]"
        if (all) b"\nunion all " else b"\nunion "
        b"select * from "
        b"\["
        buildFrom(right, None, skipParens = true)
        b"\]"
        b"\}"
      case _                        => super.expr(c)
    }
  }

  /* Extending super.InsertBuilder here instead of super.UpsertBuilder.
   INSERT OR REPLACE is almost identical to INSERT. */
  class SQLiteUpsertBuilder(ins: Insert) extends InsertBuilder(ins) {
    override protected def buildInsertStart = allNames.mkString(s"insert or replace into $tableName (", ",", ") ")
  }

  class SQLiteInsertBuilder(ins: Insert) extends InsertBuilder(ins) {
    override protected def emptyInsert: String = s"insert into $tableName default values"
  }

  class SQLiteTableDDLBuilder(table: Table[?]) extends TableDDLBuilder(table) {
    override protected val foreignKeys: Nil.type = Nil // handled directly in addTableOptions
    override protected val primaryKeys: Nil.type = Nil // handled directly in addTableOptions

    override protected def addTableOptions(b: StringBuilder): Unit = {
      for(pk <- table.primaryKeys) {
        b append ","
        addPrimaryKey(pk, b)
      }
      for(fk <- table.foreignKeys) {
        b append ","
        addForeignKey(fk, b)
      }
    }

    override def truncateTable = "delete from " + quoteTableName(tableNode)
  }

  class SQLiteColumnDDLBuilder(column: FieldSymbol) extends ColumnDDLBuilder(column) {
    override protected def appendOptions(sb: StringBuilder): Unit = {
      if(defaultLiteral ne null) sb append " DEFAULT " append defaultLiteral
      if(autoIncrement) sb append " PRIMARY KEY AUTOINCREMENT"
      else if(primaryKey) sb append " PRIMARY KEY"
      if(notNull) sb append " NOT NULL"
      if( unique ) sb append " UNIQUE"
    }
  }

  class SQLiteCountingInsertActionComposerImpl[U](compiled: CompiledInsert)
    extends CountingInsertActionComposerImpl[U](compiled) {
    // SQLite cannot perform server-side insert-or-update with soft insert semantics. We don't have to do
    // the same in ReturningInsertInvoker because SQLite does not allow returning non-AutoInc keys anyway.
    override protected val useServerSideUpsert =
      compiled.upsert.fields.forall(fs => !fs.options.contains(ColumnOption.AutoInc))
    override protected def useTransactionForUpsert = !useServerSideUpsert
  }

  override def defaultSqlTypeName(tmd: JdbcType[?], sym: Option[FieldSymbol]): String = tmd.sqlType match {
    case java.sql.Types.TINYINT | java.sql.Types.SMALLINT | java.sql.Types.BIGINT => "INTEGER"
    case _ => super.defaultSqlTypeName(tmd, sym)
  }

  class SQLiteJdbcTypes extends JdbcTypes {
    override val booleanJdbcType: SQLiteBooleanJdbcType = new SQLiteBooleanJdbcType
    override val dateJdbcType: SQLiteDateJdbcType = new SQLiteDateJdbcType
    override val localDateType: SQLiteLocalDateJdbcType = new SQLiteLocalDateJdbcType
    override val localDateTimeType: SQLiteLocalDateTimeJdbcType = new SQLiteLocalDateTimeJdbcType
    override val instantType: SQLiteInstantJdbcType = new SQLiteInstantJdbcType
    override val timeJdbcType: SQLiteTimeJdbcType = new SQLiteTimeJdbcType
    override val timestampJdbcType: SQLiteTimestampJdbcType = new SQLiteTimestampJdbcType
    override val uuidJdbcType: SQLiteUUIDJdbcType = new SQLiteUUIDJdbcType

    /* SQLite does not have a proper BOOLEAN type. The suggested workaround is
     * INTEGER with constants 1 and 0 for TRUE and FALSE. */
    class SQLiteBooleanJdbcType extends BooleanJdbcType {
      override def sqlTypeName(sym: Option[FieldSymbol]) = "INTEGER"
      override def valueToSQLLiteral(value: Boolean) = if(value) "1" else "0"
    }
    /* The SQLite JDBC driver does not support the JDBC escape syntax for
     * date/time/timestamp literals. SQLite expects these values as milliseconds
     * since epoch. */
    class SQLiteDateJdbcType extends DateJdbcType {
      override def valueToSQLLiteral(value: Date) = {
        value.getTime.toString
      }
    }
    class SQLiteLocalDateJdbcType extends LocalDateJdbcType {
      override def valueToSQLLiteral(value: LocalDate) = {
        Date.valueOf(value).getTime.toString
      }
    }
    class SQLiteInstantJdbcType extends InstantJdbcType {
      override def valueToSQLLiteral(value: Instant) = {
        value.toEpochMilli.toString
      }
    }
    class SQLiteLocalDateTimeJdbcType extends LocalDateTimeJdbcType {
      override def valueToSQLLiteral(value: LocalDateTime) = {
        Timestamp.valueOf(value).getTime.toString
      }
    }
    class SQLiteTimeJdbcType extends TimeJdbcType {
      override def valueToSQLLiteral(value: Time) = value.getTime.toString
    }
    class SQLiteTimestampJdbcType extends TimestampJdbcType {
      override def valueToSQLLiteral(value: Timestamp) = value.getTime.toString
    }
    class SQLiteUUIDJdbcType extends UUIDJdbcType {
      override def sqlType = java.sql.Types.BLOB
      override def valueToSQLLiteral(value: UUID): String =
        "x'" + value.toString.replace("-", "") + "'"
    }
  }
}

object SQLiteProfile extends SQLiteProfile
