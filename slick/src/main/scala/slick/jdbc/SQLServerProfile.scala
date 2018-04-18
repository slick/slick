package slick.jdbc

import scala.concurrent.ExecutionContext
import java.time._
import java.sql.{Date, PreparedStatement, ResultSet, Time, Timestamp}
import java.time.format.{DateTimeFormatter, DateTimeFormatterBuilder}
import java.time.temporal.ChronoField

import scala.reflect.{ClassTag, classTag}
import com.typesafe.config.Config
import slick.ast._
import slick.ast.Util._
import slick.basic.Capability
import slick.compiler._
import slick.dbio._
import slick.jdbc.meta.{MColumn, MTable}
import slick.lifted._
import slick.relational.RelationalProfile
import slick.sql.SqlCapabilities
import slick.util.{ConstArray, GlobalConfig, SlickLogger}
import slick.util.MacroSupport.macroSupportInterpolation
import slick.util.ConfigExtensionMethods._

/** Slick profile for Microsoft SQL Server.
  *
  * This profile implements [[slick.jdbc.JdbcProfile]]
  * ''without'' the following capabilities:
  *
  * <ul>
  *   <li>[[slick.jdbc.JdbcCapabilities.returnInsertOther]]:
  *     When returning columns from an INSERT operation, only a single column
  *     may be specified which must be the table's AutoInc column.</li>
  *   <li>[[slick.sql.SqlCapabilities.sequence]]:
  *     Sequences are not supported because SQLServer does not have this
  *     feature.</li>
  *   <li>[[slick.jdbc.JdbcCapabilities.forceInsert]]:
  *     Inserting explicit values into AutoInc columns with ''forceInsert''
  *     operations is not supported.</li>
  *   <li>[[slick.jdbc.JdbcCapabilities.createModel]]:
  *     Reading the database schema is currently only supported through JTDS,
  *     not through Microsoft's official JDBC driver.</li>
  *   <li>[[slick.jdbc.JdbcCapabilities.insertOrUpdate]]:
  *     InsertOrUpdate operations are emulated on the client side if generated
  *     keys should be returned. Otherwise the operation is performmed
  *     natively on the server side.</li>
  *   <li>[[slick.jdbc.JdbcCapabilities.supportsByte]]:
  *     SQL Server's TINYINT is unsigned. It doesn't have a signed Byte-like
  *     type. Slick maps Byte to SMALLINT instead and that's how it shows up
  *     in model and code-generation.</li>
  * </ul>
  *
  * The default type for strings of unlimited length is "VARCHAR(MAX)", falling back to
  * "VARCHAR(254)" if a `PrimaryKey` column option is set. This can be
  * changed by overriding <code>slick.jdbc.SQLServerProfile.defaultStringType</code>
  * in application.conf.
  */
trait SQLServerProfile extends JdbcProfile {

  override protected[this] def loadProfileConfig: Config = {
    if(!GlobalConfig.profileConfig("slick.driver.SQLServer").entrySet().isEmpty)
      SlickLogger[SQLServerProfile].warn("The config key 'slick.driver.SQLServer' is deprecated and not used anymore. Use 'slick.jdbc.SQLServerProfile' instead.")
    super.loadProfileConfig
  }

  protected lazy val defaultStringType = profileConfig.getStringOpt("defaultStringType")

  override protected def computeCapabilities: Set[Capability] = (super.computeCapabilities
    - JdbcCapabilities.forceInsert
    - JdbcCapabilities.returnInsertOther
    - JdbcCapabilities.insertOrUpdate
    - SqlCapabilities.sequence
    - JdbcCapabilities.supportsByte
  )

  override protected def computeQueryCompiler =
    (super.computeQueryCompiler
      .addAfter(new RemoveTakeDrop(translateTake = false), Phase.expandSums)
      .addBefore(new ProtectGroupBy, Phase.mergeToComprehensions)
      .replace(new RemoveFieldNames(alwaysKeepSubqueryNames = true))
      + Phase.rewriteBooleans)
  override protected lazy val useServerSideUpsert = true
  override protected lazy val useServerSideUpsertReturning = false
  override val columnTypes = new JdbcTypes
  override def createQueryBuilder(n: Node, state: CompilerState): QueryBuilder = new QueryBuilder(n, state)
  override def createInsertBuilder(node: Insert): super.InsertBuilder = new InsertBuilder(node)
  override def createUpsertBuilder(node: Insert): super.InsertBuilder = new UpsertBuilder(node)
  override def createTableDDLBuilder(table: Table[_]): TableDDLBuilder = new TableDDLBuilder(table)
  override def createColumnDDLBuilder(column: FieldSymbol, table: Table[_]): ColumnDDLBuilder = new ColumnDDLBuilder(column)

  class ModelBuilder(mTables: Seq[MTable], ignoreInvalidDefaults: Boolean)(implicit ec: ExecutionContext) extends JdbcModelBuilder(mTables, ignoreInvalidDefaults) {
    override def createColumnBuilder(tableBuilder: TableBuilder, meta: MColumn): ColumnBuilder = new ColumnBuilder(tableBuilder, meta) {
      override def tpe = dbType match {
        case Some("date") => "java.sql.Date"
        case Some("time") => "java.sql.Time"
        case Some("datetime2") => "java.sql.Timestamp"
        case _ => super.tpe
      }
      override def rawDefault = super.rawDefault.map(_.stripPrefix("(") // jtds
                                                      .stripPrefix("(")
                                                      .stripSuffix(")")
                                                      .stripSuffix(")"))
      override def default = rawDefault.map((_,tpe)).collect{
        case ("0","Boolean")  => Some(false)
        case ("1","Boolean")  => Some(true)
      }.map(d => Some(d)).getOrElse{super.default}
    }
    override def jdbcTypeToScala(jdbcType: Int, typeName: String = ""): ClassTag[_] = {
      //SQL Server's TINYINT type is unsigned while Scala's Byte is signed
      if( jdbcType == java.sql.Types.TINYINT )
        classTag[Short]
      else
        super.jdbcTypeToScala( jdbcType , typeName )
    }
  }

  override def createModelBuilder(tables: Seq[MTable], ignoreInvalidDefaults: Boolean)(implicit ec: ExecutionContext): JdbcModelBuilder =
    new ModelBuilder(tables, ignoreInvalidDefaults)

  override def defaultTables(implicit ec: ExecutionContext): DBIO[Seq[MTable]] = {
    MTable.getTables(None, None, None, Some(Seq("TABLE"))).map(_.filter(!_.name.schema.contains("sys")))
  }

  override def defaultSqlTypeName(tmd: JdbcType[_], sym: Option[FieldSymbol]): String = tmd.sqlType match {
    case java.sql.Types.VARCHAR =>
      sym.flatMap(_.findColumnOption[RelationalProfile.ColumnOption.Length]) match {
        case Some(l) => if(l.varying) s"VARCHAR(${l.length})" else s"CHAR(${l.length})"
        case None => defaultStringType match {
          case Some(s) => s
          case None =>
            if(sym.flatMap(_.findColumnOption[ColumnOption.PrimaryKey.type]).isDefined)
              "VARCHAR(254)" else "VARCHAR(MAX)"
        }
      }
    case java.sql.Types.BOOLEAN => "BIT"
    case java.sql.Types.BLOB => "VARBINARY(MAX)"
    case java.sql.Types.CLOB => "TEXT"
    case java.sql.Types.DOUBLE => "FLOAT(53)"
    case java.sql.Types.FLOAT => "FLOAT(24)"
    case _ => super.defaultSqlTypeName(tmd, sym)
  }

  class QueryBuilder(tree: Node, state: CompilerState) extends super.QueryBuilder(tree, state) {
    override protected val supportsTuples = false
    override protected val concatOperator = Some("+")

    override protected def buildSelectModifiers(c: Comprehension): Unit = {
      super.buildSelectModifiers(c)
      (c.fetch, c.offset) match {
        case (Some(t), Some(d)) => b"top (${QueryParameter.constOp[Long]("+")(_ + _)(t, d)}) "
        case (Some(t), None   ) => b"top ($t) "
        case (None,    _      ) => if(!c.orderBy.isEmpty) b"top 100 percent "
      }
    }

    override protected def buildFetchOffsetClause(fetch: Option[Node], offset: Option[Node]) = ()

    override protected def buildOrdering(n: Node, o: Ordering): Unit = {
      if(o.nulls.last && !o.direction.desc)
        b"case when ($n) is null then 1 else 0 end,"
      else if(o.nulls.first && o.direction.desc)
        b"case when ($n) is null then 0 else 1 end,"
      expr(n)
      if(o.direction.desc) b" desc"
    }

    override protected def buildFromClause(from: Seq[(TermSymbol, Node)]) = {
      super.buildFromClause(from)
      tree match {
        // SQL Server "select for update" syntax
        case c: Comprehension => if(c.forUpdate) b" with (updlock,rowlock) "
        case _ =>
      }
    }

    override protected def buildForUpdateClause(forUpdate: Boolean) = {
      // SQLSever doesn't have "select for update" syntax, so use with (updlock,rowlock) in from clause
    }

    override def expr(n: Node, skipParens: Boolean = false): Unit = n match {
      // Cast bind variables of type TIME to TIME (otherwise they're treated as TIMESTAMP)
      case c @ LiteralNode(v) if c.volatileHint && jdbcTypeFor(c.nodeType) == columnTypes.timeJdbcType =>
        b"cast("
        super.expr(n, skipParens)
        b" as ${columnTypes.timeJdbcType.sqlTypeName(None)})"
      case QueryParameter(extractor, tpe, _) if jdbcTypeFor(tpe) == columnTypes.timeJdbcType =>
        b"cast("
        super.expr(n, skipParens)
        b" as ${columnTypes.timeJdbcType.sqlTypeName(None)})"
      case Library.Substring(n, start) =>
        b"\({fn substring($n, ${QueryParameter.constOp[Int]("+")(_ + _)(start, LiteralNode(1).infer())}, ${Int.MaxValue})}\)"
      case Library.Repeat(str, count) =>
        b"replicate($str, $count)"
      case n => super.expr(n, skipParens)
    }
  }

  class InsertBuilder(ins: Insert) extends super.InsertBuilder(ins) {
    override protected def emptyInsert: String = s"insert into $tableName default values"
  }

  class UpsertBuilder(ins: Insert) extends super.UpsertBuilder(ins) {
    // SQL Server requires MERGE statements to end with a semicolon (unlike all other
    // statements that you can execute via JDBC)
    override protected def buildMergeEnd: String = super.buildMergeEnd + ";"
  }

  class TableDDLBuilder(table: Table[_]) extends super.TableDDLBuilder(table) {
    override protected def addForeignKey(fk: ForeignKey, sb: StringBuilder): Unit = {
      val updateAction = fk.onUpdate.action
      val deleteAction = fk.onDelete.action
      sb append "constraint " append quoteIdentifier(fk.name) append " foreign key("
      addForeignKeyColumnList(fk.linearizedSourceColumns, sb, tableNode.tableName)
      sb append ") references " append quoteTableName(fk.targetTable) append "("
      addForeignKeyColumnList(fk.linearizedTargetColumnsForOriginalTargetTable, sb, fk.targetTable.tableName)
      // SQLServer has no RESTRICT. Equivalent is NO ACTION. http://technet.microsoft.com/en-us/library/aa902684%28v=sql.80%29.aspx
      sb append ") on update " append (if(updateAction == "RESTRICT") "NO ACTION" else updateAction)
      sb append " on delete " append (if(deleteAction == "RESTRICT") "NO ACTION" else deleteAction)
    }

    override def dropIfExistsPhase = {
      //http://stackoverflow.com/questions/7887011/how-to-drop-a-table-if-it-exists-in-sql-server
      Iterable(
      "IF EXISTS (SELECT 1 FROM sys.objects WHERE object_id = OBJECT_ID(N'"
      + (tableNode.schemaName match{
        case Some(s)=>s+"."
        case None=>""
      })
      + tableNode.tableName
      + "') AND type in (N'U'))\n"
      + "begin\n"
      + dropPhase1.mkString("\n") + dropPhase2.mkString("\n")
      + "\nend")
    }

    override def createIfNotExistsPhase = {
      //http://stackoverflow.com/questions/5952006/how-to-check-if-table-exist-and-if-it-doesnt-exist-create-table-in-sql-server-2
      Iterable(
      "IF  NOT EXISTS (SELECT 1 FROM sys.objects WHERE object_id = OBJECT_ID(N'"
      + (tableNode.schemaName match{
        case Some(s)=>s+"."
        case None=>""
      })
      + tableNode.tableName
      + "') AND type in (N'U'))\n"
      + "begin\n"
      + createPhase1.mkString("\n") + createPhase2.mkString("\n")
      + "\nend")
    }
  }

  class ColumnDDLBuilder(column: FieldSymbol) extends super.ColumnDDLBuilder(column) {
    override protected def appendOptions(sb: StringBuilder): Unit = {
      if(defaultLiteral ne null) sb append " DEFAULT " append defaultLiteral
      if(notNull) sb append " NOT NULL"
      if(primaryKey) sb append " PRIMARY KEY"
      if(autoIncrement) sb append " IDENTITY"
      if( unique ) sb append " UNIQUE"
    }
  }

  class JdbcTypes extends super.JdbcTypes {
    override val booleanJdbcType = new BooleanJdbcType
    override val byteJdbcType = new ByteJdbcType
    override val byteArrayJdbcType = new ByteArrayJdbcType
    override val dateJdbcType = new DateJdbcType
    override val timeJdbcType = new TimeJdbcType
    override val localTimeType = new LocalTimeJdbcType
    override val timestampJdbcType = new TimestampJdbcType
    override val localDateTimeType = new LocalDateTimeJdbcType
    override val instantType = new InstantJdbcType
    override val offsetDateTimeType = new OffsetDateTimeJdbcType
    override val uuidJdbcType = new UUIDJdbcType {
      override def sqlTypeName(sym: Option[FieldSymbol]) = "UNIQUEIDENTIFIER"
    }
    /* SQL Server does not have a proper BOOLEAN type. The suggested workaround is
     * BIT with constants 1 and 0 for TRUE and FALSE. */
    class BooleanJdbcType extends super.BooleanJdbcType {
      override def valueToSQLLiteral(value: Boolean) = if(value) "1" else "0"
    }
    /* Selecting a straight Date or Timestamp literal fails with a NPE (probably
     * because the type information gets lost along the way), so we cast all Date
     * and Timestamp values to the proper type. This work-around does not seem to
     * be required for Time values. */
    /* TIMESTAMP in SQL Server is a data type for sequence numbers. What we
     * want is DATETIME2. */
    class DateJdbcType extends super.DateJdbcType {
      override def valueToSQLLiteral(value: Date) = "(convert(date, {d '" + value + "'}))"
    }
    class TimeJdbcType extends super.TimeJdbcType {
      override def valueToSQLLiteral(value: Time) = "(convert(time, {t '" + value + "'}))"
      override def getValue(r: ResultSet, idx: Int) = {
        r.getString(idx) match {
          case null => null
          case serializedTime =>
            val sep = serializedTime.indexOf('.')
            if (sep == -1) Time.valueOf(serializedTime)
            else {
              val t = Time.valueOf(serializedTime.substring(0, sep))
              val millis = (("0." + serializedTime.substring(sep + 1)).toDouble * 1000.0).toInt
              t.setTime(t.getTime + millis)
              t
            }
        }
      }
    }

    class LocalTimeJdbcType extends super.LocalTimeJdbcType {
      private[this] val formatter : DateTimeFormatter = {
        new DateTimeFormatterBuilder()
          .append(DateTimeFormatter.ofPattern("HH:mm:ss"))
          .optionalStart()
          .appendFraction(ChronoField.NANO_OF_SECOND, 0, 6, true)
          .optionalEnd()
          .toFormatter()
      }
      override def sqlTypeName(sym: Option[FieldSymbol]) = "TIME(6)"
      override def getValue(r: ResultSet, idx: Int) = {
        r.getString(idx) match {
          case null => null
          case serializedTime =>

            val sep = serializedTime.indexOf('.')
            if (sep == -1) {
              Time.valueOf(serializedTime).toLocalTime
            } else {
              LocalTime.parse(serializedTime, formatter)
            }
        }
      }
      override def valueToSQLLiteral(value: LocalTime) = {
        s"(convert(time(6), '$value'))"
      }
    }
    class TimestampJdbcType extends super.TimestampJdbcType {
      override def sqlTypeName(sym: Option[FieldSymbol]) = "DATETIME2(6)"
      override def valueToSQLLiteral(value: Timestamp) = "(convert(datetime, {ts '" + value + "'}))"
    }
    class LocalDateTimeJdbcType extends super.LocalDateTimeJdbcType {
      private[this] val formatter : DateTimeFormatter = {
        new DateTimeFormatterBuilder()
          .append(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss"))
          .optionalStart()
          .appendFraction(ChronoField.NANO_OF_SECOND, 0, 6, true)
          .optionalEnd()
          .toFormatter()
      }
      override def sqlTypeName(sym: Option[FieldSymbol]) = "DATETIME2(6)"
      override def getValue(r: ResultSet, idx: Int): LocalDateTime = {
        r.getTimestamp(idx) match {
          case null =>
            null
          case timestamp =>
            timestamp.toLocalDateTime
        }
      }
    }
    class InstantJdbcType extends super.InstantJdbcType {
      private[this] val formatter : DateTimeFormatter = {
        new DateTimeFormatterBuilder()
          .append(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss"))
          .appendFraction(ChronoField.NANO_OF_SECOND, 0, 6, true)
          .appendPattern(" ")
          .appendOffset("+HH:MM", "")
          .toFormatter()
      }
      private[this] def serializeInstantValue(value : Instant) : String = {
        formatter.format(
          OffsetDateTime.ofInstant(value, ZoneOffset.UTC)
        )
      }
      override def sqlTypeName(sym: Option[FieldSymbol]) = "DATETIMEOFFSET(6)"
      override def setValue(v: Instant, p: PreparedStatement, idx: Int) : Unit = {
        p.setString(idx, serializeInstantValue(v))
      }
      override def updateValue(v: Instant, r: ResultSet, idx: Int) : Unit = {
        r.updateString(idx, serializeInstantValue(v))
      }

      override def getValue(r: ResultSet, idx: Int): Instant = {
        r.getString(idx) match {
          case null =>
            null
          case dateStr =>
            OffsetDateTime.parse(dateStr, formatter).toInstant()
        }
      }
      override def valueToSQLLiteral(value: Instant) = {
        s"(convert(datetimeoffset(6), '${serializeInstantValue(value)}'))"
      }
    }
    class OffsetDateTimeJdbcType extends super.OffsetDateTimeJdbcType {
      override def sqlTypeName(sym: Option[FieldSymbol]) = "DATETIMEOFFSET(6)"

      private[this] val formatter: DateTimeFormatter = {
        new DateTimeFormatterBuilder()
          .append(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss"))
          .optionalStart()
          .appendFraction(ChronoField.NANO_OF_SECOND, 0, 6, true)
          .optionalEnd()
          .appendLiteral(' ')
          .appendOffsetId()
          .toFormatter()
      }
      override def getValue(r: ResultSet, idx: Int): OffsetDateTime = {
        r.getString(idx) match {
          case null =>
            null
          case timestamp =>
            OffsetDateTime.parse(timestamp, formatter)
        }
      }
    }
    /* SQL Server's TINYINT is unsigned, so we use SMALLINT instead to store a signed byte value.
     * The JDBC driver also does not treat signed values correctly when reading bytes from result
     * sets, so we read as Short and then convert to Byte. */
    class ByteJdbcType extends super.ByteJdbcType {
      override def sqlTypeName(sym: Option[FieldSymbol]) = "SMALLINT"
      override def getValue(r: ResultSet, idx: Int) = r.getShort(idx).toByte
    }
    /* SQL Server supports a literal notation for byte arrays */
    private[this] val hexChars = "0123456789ABCDEF".toCharArray()
    class ByteArrayJdbcType extends super.ByteArrayJdbcType {
      override def hasLiteralForm = true
      override def valueToSQLLiteral(value: Array[Byte]) = "0x" +  bytesToHex(value)
      private[this] def bytesToHex(bytes: Array[Byte]) = {
        val a = new Array[Char](bytes.length * 2)
        var j = 0
        while(j < bytes.length) {
          val v = bytes(j) & 0xFF
          a(j*2) = hexChars(v >>> 4)
          a(j*2 + 1) = hexChars(v & 0x0F)
          j += 1
        }
        new String(a)
      }
    }
  }
}

object SQLServerProfile extends SQLServerProfile

/** Ensure that every expression in a GroupBy's "by" clause contains a reference to a proper
  * source field. If this is not the case, wrap the source in a Subquery boundary. */
class ProtectGroupBy extends Phase {
  val name = "protectGroupBy"

  def apply(state: CompilerState) = state.map(_.replace({
    case n @ Bind(s1, g1 @ GroupBy(s2, f1, b1, ts1), Pure(str1, ts2)) =>
      logger.debug("Examining GroupBy", g1)
      val (b2, b2s) = source(s2, b1, f1)
      logger.debug(s"Narrowed 'by' clause down to: (over $b2s)", b2)
      val refsOK = ProductNode(ConstArray(b2)).flatten.children.forall(_.findNode {
        case Ref(s) if s == b2s => true
        case _ => false
      }.isDefined)
      logger.debug("All columns reference the source: "+refsOK)
      if(refsOK) n
      else n.copy(from = g1.copy(from = Subquery(f1, Subquery.Default))).infer()

  }, bottomUp = true, keepType = true))

  def source(bs: TermSymbol, b: Node, n: Node): (Node, TermSymbol) = n match {
    case Filter(_, f, _) => source(bs, b, f)
    case CollectionCast(f, _) => source(bs, b, f)
    case Bind(s, f, Pure(StructNode(defs), _)) =>
      val m = defs.toMap
      val b2 = b.replace({
        case Select(Ref(s), f) if s == bs => m(f)
      }, keepType = true)
      source(s, b2, f)
    case _ => (b, bs)
  }
}
