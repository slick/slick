package slick.jdbc

import java.time.format.{DateTimeFormatterBuilder, DateTimeFormatter}
import java.time.temporal.ChronoField
import java.time._
import java.util.UUID
import java.sql.{PreparedStatement, ResultSet}

import scala.concurrent.ExecutionContext

import slick.ast._
import slick.basic.Capability
import slick.compiler.{Phase, CompilerState}
import slick.dbio._
import slick.jdbc.meta.{MIndexInfo, MColumn, MTable}
import slick.relational.RelationalProfile
import slick.util.ConstArray
import slick.util.MacroSupport.macroSupportInterpolation

/** Slick profile for PostgreSQL.
  *
  * This profile implements [[slick.jdbc.JdbcProfile]]
  * ''without'' the following capabilities:
  *
  * <ul>
  *   <li>[[slick.jdbc.JdbcCapabilities.insertOrUpdate]]:
  *     InsertOrUpdate operations are emulated on the server side with a single
  *     JDBC statement executing multiple server-side statements in a transaction.
  *     This is faster than a client-side emulation but may still fail due to
  *     concurrent updates. InsertOrUpdate operations with `returning` are
  *     emulated on the client side.</li>
  *   <li>[[slick.jdbc.JdbcCapabilities.nullableNoDefault]]:
  *     Nullable columns always have NULL as a default according to the SQL
  *     standard. Consequently Postgres treats no specifying a default value
  *     just as specifying NULL and reports NULL as the default value.
  *     Some other dbms treat queries with no default as NULL default, but
  *     distinguish NULL from no default value in the meta data.</li>
  *   <li>[[slick.jdbc.JdbcCapabilities.supportsByte]]:
  *     Postgres doesn't have a corresponding type for Byte.
  *     SMALLINT is used instead and mapped to Short in the Slick model.</li>
  * </ul>
  *
  * Notes:
  *
  * <ul>
  *   <li>[[slick.relational.RelationalCapabilities.typeBlob]]:
  *   The default implementation of the <code>Blob</code> type uses the
  *   database type <code>lo</code> and the stored procedure
  *   <code>lo_manage</code>, both of which are provided by the "lo"
  *   extension in PostgreSQL.</li>
  * </ul>
  */
trait PostgresProfile extends JdbcProfile {

  override protected def computeCapabilities: Set[Capability] = (super.computeCapabilities
    - JdbcCapabilities.insertOrUpdate
    - JdbcCapabilities.nullableNoDefault
    - JdbcCapabilities.supportsByte
  )

  class ModelBuilder(mTables: Seq[MTable], ignoreInvalidDefaults: Boolean)(implicit ec: ExecutionContext) extends JdbcModelBuilder(mTables, ignoreInvalidDefaults) {
    override def createTableNamer(mTable: MTable): TableNamer = new TableNamer(mTable) {
      override def schema = super.schema.filter(_ != "public") // remove default schema
    }
    override def createColumnBuilder(tableBuilder: TableBuilder, meta: MColumn): ColumnBuilder = new ColumnBuilder(tableBuilder, meta) {
      /*
      The default value for numeric type behave different with postgres version
      PG9.5 - PG9.6:
       positive default value in int boundary: 1
       negative default value in int boundary: '-1'::integer
       positive default value between int boundary and long boundary: '123123214232131312'::bitint
       negative default value between int boundary and long boundary: '-123123214232131312'::bitint
       positive default value beyond long boundary: '111111111111111111111111111'::numeric
       negative default value beyond long boundary: '-111111111111111111111111111'::numeric
       positive floating: '1.1'::numeric
       negative floating: '-.1.1'::numeric

      PGX.X to PG9.4:
       positive default value in int boundary: 1
       negative default value in int boundary: (-1)
       positive default value between int boundary and long boundary: 123123214232131312::bitint
       negative default value between int boundary and long boundary: (-123123214232131312)::bitint
       positive default value beyond long boundary: 111111111111111111111111111::numeric
       negative default value beyond long boundary: (-111111111111111111111111111)::numeric
       positive floating: 1.1
       negative floating: (-.1.1)


       */
      val NumericPattern = "^['(]?(-?[0-9]+\\.?[0-9]*)[')]?(?:::(?:numeric|bigint|integer))?".r
      val TextPattern = "^'(.*)'::(?:bpchar|character varying|text)".r
      val UUIDPattern = "^'(.*)'::uuid".r
      override def default = meta.columnDef.map((_,tpe)).collect{
        case ("true","Boolean")  => Some(Some(true))
        case ("false","Boolean") => Some(Some(false))
        case (TextPattern(str),"String") => Some(Some(str))
        case ("NULL::bpchar", "String") => Some(None)
        case (TextPattern(str),"Char") => str.length match {
          case 0 => Some(Some(' ')) // Default to one space, as the char will be space padded anyway
          case 1 => Some(Some(str.head))
          case _ => None // This is invalid, so let's not supply any default
        }
        case ("NULL::bpchar", "Char") => Some(None)
        case (NumericPattern(v),"Short") => Some(Some(v.toShort))
        case (NumericPattern(v),"Int") => Some(Some(v.toInt))
        case (NumericPattern(v),"Long") => Some(Some(v.toLong))
        case (NumericPattern(v),"Float") => Some(Some(v.toFloat))
        case (NumericPattern(v),"Double") => Some(Some(v.toDouble))
        case (NumericPattern(v), "scala.math.BigDecimal") => Some(Some(BigDecimal(s"$v")))
        case (UUIDPattern(v),"java.util.UUID") => Some(Some(java.util.UUID.fromString(v)))
        case (_,"java.util.UUID") => None // The UUID is generated through a function - treat it as if there was no default.
      }.getOrElse{
        val d = super.default
        if(meta.nullable == Some(true) && d == None){
          Some(None)
        } else d
      }
      override def length: Option[Int] = {
        val l = super.length
        if(tpe == "String" && varying && l == Some(2147483647)) None
        else l
      }
      override def tpe = meta.typeName match {
        case "bytea" => "Array[Byte]"
        case "lo" if meta.sqlType == java.sql.Types.DISTINCT => "java.sql.Blob"
        case "uuid" => "java.util.UUID"
        case _ => super.tpe
      }
    }
    override def createIndexBuilder(tableBuilder: TableBuilder, meta: Seq[MIndexInfo]): IndexBuilder = new IndexBuilder(tableBuilder, meta) {
      // FIXME: this needs a test
      override def columns = super.columns.map(_.stripPrefix("\"").stripSuffix("\""))
    }
  }

  override def createModelBuilder(tables: Seq[MTable], ignoreInvalidDefaults: Boolean)(implicit ec: ExecutionContext): JdbcModelBuilder =
    new ModelBuilder(tables, ignoreInvalidDefaults)

  override def defaultTables(implicit ec: ExecutionContext): DBIO[Seq[MTable]] =
    MTable.getTables(None, None, None, Some(Seq("TABLE")))

  override val columnTypes = new JdbcTypes
  override protected def computeQueryCompiler = super.computeQueryCompiler - Phase.rewriteDistinct
  override def createQueryBuilder(n: Node, state: CompilerState): QueryBuilder = new QueryBuilder(n, state)
  override def createUpsertBuilder(node: Insert): InsertBuilder = new UpsertBuilder(node)
  override def createTableDDLBuilder(table: Table[_]): TableDDLBuilder = new TableDDLBuilder(table)
  override def createColumnDDLBuilder(column: FieldSymbol, table: Table[_]): ColumnDDLBuilder = new ColumnDDLBuilder(column)
  override protected lazy val useServerSideUpsert = true
  override protected lazy val useTransactionForUpsert = true
  override protected lazy val useServerSideUpsertReturning = false

  override def defaultSqlTypeName(tmd: JdbcType[_], sym: Option[FieldSymbol]): String = tmd.sqlType match {
    case java.sql.Types.VARCHAR =>
      val size = sym.flatMap(_.findColumnOption[RelationalProfile.ColumnOption.Length])
      size.fold("VARCHAR")(l => if(l.varying) s"VARCHAR(${l.length})" else s"CHAR(${l.length})")
    case java.sql.Types.BLOB => "lo"
    case java.sql.Types.DOUBLE => "DOUBLE PRECISION"
    /* PostgreSQL does not have a TINYINT type, so we use SMALLINT instead. */
    case java.sql.Types.TINYINT => "SMALLINT"
    case _ => super.defaultSqlTypeName(tmd, sym)
  }

  class QueryBuilder(tree: Node, state: CompilerState) extends super.QueryBuilder(tree, state) {
    override protected val concatOperator = Some("||")
    override protected val quotedJdbcFns = Some(Vector(Library.Database, Library.User))

    override protected def buildSelectModifiers(c: Comprehension): Unit = (c.distinct, c.select) match {
      case (Some(ProductNode(onNodes)), Pure(ProductNode(selNodes), _)) if onNodes.nonEmpty =>
        def eligible(a: ConstArray[Node]) = a.forall {
          case _: PathElement => true
          case _: LiteralNode => true
          case _: QueryParameter => true
          case _ => false
        }
        if(eligible(onNodes) && eligible(selNodes) &&
          onNodes.iterator.collect[List[TermSymbol]] { case FwdPath(ss) => ss }.toSet ==
            selNodes.iterator.collect[List[TermSymbol]] { case FwdPath(ss) => ss }.toSet
        ) b"distinct " else super.buildSelectModifiers(c)
      case _ => super.buildSelectModifiers(c)
    }

    override protected def buildFetchOffsetClause(fetch: Option[Node], offset: Option[Node]) = (fetch, offset) match {
      case (Some(t), Some(d)) => b"\nlimit $t offset $d"
      case (Some(t), None   ) => b"\nlimit $t"
      case (None,    Some(d)) => b"\noffset $d"
      case _ =>
    }

    override def expr(n: Node, skipParens: Boolean = false) = n match {
      case Library.UCase(ch) => b"upper($ch)"
      case Library.LCase(ch) => b"lower($ch)"
      case Library.IfNull(ch, d) => b"coalesce($ch, $d)"
      case Library.NextValue(SequenceNode(name)) => b"nextval('$name')"
      case Library.CurrentValue(SequenceNode(name)) => b"currval('$name')"
      case Library.CurrentDate() => b"current_date"
      case Library.CurrentTime() => b"current_time"
      case Union(left, right, all) =>
        b"\{"
        buildFrom(left, None)
        if (all) b"\nunion all " else b"\nunion "
        buildFrom(right, None)
        b"\}"
      case _ => super.expr(n, skipParens)
    }
  }

  class UpsertBuilder(ins: Insert) extends super.UpsertBuilder(ins) {
    override def buildInsert: InsertBuilderResult = {
      val update = "update " + tableName + " set " + softNames.map(n => s"$n=?").mkString(",") + " where " + pkNames.map(n => s"$n=?").mkString(" and ")
      val nonAutoIncNames = nonAutoIncSyms.map(fs => quoteIdentifier(fs.name)).mkString(",")
      val nonAutoIncVars = nonAutoIncSyms.map(_ => "?").mkString(",")
      val cond = pkNames.map(n => s"$n=?").mkString(" and ")
      val insert = s"insert into $tableName ($nonAutoIncNames) select $nonAutoIncVars where not exists (select 1 from $tableName where $cond)"
      new InsertBuilderResult(table, s"$update; $insert", ConstArray.from(softSyms ++ pkSyms))
    }

    override def transformMapping(n: Node) = reorderColumns(n, softSyms ++ pkSyms ++ nonAutoIncSyms.toSeq ++ pkSyms)
  }

  class TableDDLBuilder(table: Table[_]) extends super.TableDDLBuilder(table) {
    override def createPhase1 = super.createPhase1 ++ columns.flatMap {
      case cb: ColumnDDLBuilder => cb.createLobTrigger(table.tableName)
    }
    override def dropPhase1 = {
      val dropLobs = columns.flatMap {
        case cb: ColumnDDLBuilder => cb.dropLobTrigger(table.tableName)
      }
      if(dropLobs.isEmpty) super.dropPhase1
      else Seq("delete from "+quoteIdentifier(table.tableName)) ++ dropLobs ++ super.dropPhase1
    }
  }

  class ColumnDDLBuilder(column: FieldSymbol) extends super.ColumnDDLBuilder(column) {
    override def appendColumn(sb: StringBuilder): Unit = {
      sb append quoteIdentifier(column.name) append ' '
      if(autoIncrement && !customSqlType) {
        sb append (if(sqlType.toUpperCase == "BIGINT") "BIGSERIAL" else "SERIAL")
      } else appendType(sb)
      autoIncrement = false
      appendOptions(sb)
    }

    def lobTrigger(tname: String) =
      quoteIdentifier(tname+"__"+quoteIdentifier(column.name)+"_lob")

    def createLobTrigger(tname: String): Option[String] =
      if(sqlType == "lo") Some(
        "create trigger "+lobTrigger(tname)+" before update or delete on "+
        quoteIdentifier(tname)+" for each row execute procedure lo_manage("+quoteIdentifier(column.name)+")"
      ) else None

    def dropLobTrigger(tname: String): Option[String] =
      if(sqlType == "lo") Some(
        "drop trigger "+lobTrigger(tname)+" on "+quoteIdentifier(tname)
      ) else None
  }

  class JdbcTypes extends super.JdbcTypes {
    override val byteArrayJdbcType = new ByteArrayJdbcType
    override val uuidJdbcType = new UUIDJdbcType
    override val localDateType = new LocalDateJdbcType
    override val localTimeType = new LocalTimeJdbcType
    override val offsetTimeType = new OffsetTimeJdbcType
    //OffsetDateTime and ZonedDateTime not currently supportable natively by the backend
    override val instantType = new InstantJdbcType
    override val localDateTimeType = new LocalDateTimeJdbcType

    class ByteArrayJdbcType extends super.ByteArrayJdbcType {
      override val sqlType = java.sql.Types.BINARY
      override def sqlTypeName(sym: Option[FieldSymbol]) = "BYTEA"
    }

    trait PostgreTimeJdbcType [T] {

      implicit val min : T
      implicit val max : T
      implicit val serializeFiniteTime : (T => String)
      implicit val parseFiniteTime : (String => T)

      @inline
      private[this] val negativeInfinite = "-infinity"
      @inline
      private[this] val positiveInfinite = "infinity"

      protected def serializeTime(time : T): String = {
        time match {
          case null => null
          case `min` => negativeInfinite
          case `max` => positiveInfinite
          case _ => serializeFiniteTime(time)
        }
      }
      protected def parseTime(time : String): T = {
        time match {
          case null => null.asInstanceOf[T]
          case `negativeInfinite` => max
          case `positiveInfinite` => min
          case _ => parseFiniteTime(time)
        }
      }
    }

    import PGUtils.createPGObject
    class LocalDateJdbcType extends super.LocalDateJdbcType with PostgreTimeJdbcType[LocalDate] {

      private[this] val formatter = DateTimeFormatter.ISO_LOCAL_DATE

      implicit val min : LocalDate = LocalDate.MIN
      implicit val max : LocalDate = LocalDate.MAX
      implicit val serializeFiniteTime : (LocalDate => String) =  _.format(formatter)
      implicit val parseFiniteTime : (String => LocalDate) = LocalDate.parse(_, formatter)

      override val sqlType = java.sql.Types.DATE
      override def sqlTypeName(sym: Option[FieldSymbol]) = "DATE"
      override def getValue(r: ResultSet, idx: Int): LocalDate = parseTime(r.getString(idx))
      override def setValue(v: LocalDate, p: PreparedStatement, idx: Int) = {
        p.setObject(idx, serializeTime(v), sqlType)
      }
      override def updateValue(v: LocalDate, r: ResultSet, idx: Int) = {
        r.updateObject(idx, createPGObject(serializeTime(v), sqlTypeName(None)))
      }
      override val hasLiteralForm : Boolean = false
    }

    class LocalTimeJdbcType extends super.LocalTimeJdbcType with PostgreTimeJdbcType[LocalTime] {

      private[this] val formatter : DateTimeFormatter = DateTimeFormatter.ISO_LOCAL_TIME

      implicit val min : LocalTime = LocalTime.MIN
      implicit val max : LocalTime = LocalTime.MAX
      implicit val serializeFiniteTime : (LocalTime => String) =  _.format(formatter)
      implicit val parseFiniteTime : (String => LocalTime) = LocalTime.parse(_, formatter)

      override val sqlType = java.sql.Types.OTHER
      override def sqlTypeName(sym: Option[FieldSymbol]) = "TIME"
      override def setValue(v: LocalTime, p: PreparedStatement, idx: Int) = {
        p.setObject(idx, serializeTime(v), sqlType)
      }
      override def updateValue(v: LocalTime, r: ResultSet, idx: Int) = {
        r.updateObject(idx, createPGObject(serializeTime(v), sqlTypeName(None)))
      }
      override def getValue(r: ResultSet, idx: Int): LocalTime = parseTime(r.getString(idx))
      override val hasLiteralForm : Boolean = false
    }

    class OffsetTimeJdbcType extends super.OffsetTimeJdbcType with PostgreTimeJdbcType[OffsetTime] {

      private[this] val formatter : DateTimeFormatter = {
        new DateTimeFormatterBuilder()
          .append(DateTimeFormatter.ofPattern("HH:mm:ss"))
          .optionalStart()
          .appendFraction(ChronoField.NANO_OF_SECOND, 0, 6, true)
          .optionalEnd()
          .appendOffset("+HH:mm", "+00")
          .toFormatter()
      }

      implicit val min : OffsetTime = OffsetTime.MIN
      implicit val max : OffsetTime = OffsetTime.MAX
      implicit val serializeFiniteTime : (OffsetTime => String) =  _.format(formatter)
      implicit val parseFiniteTime : (String => OffsetTime) = OffsetTime.parse(_, formatter)

      override val sqlType = java.sql.Types.OTHER
      override def sqlTypeName(sym: Option[FieldSymbol]) = "TIMETZ"
      override def setValue(v: OffsetTime, p: PreparedStatement, idx: Int) = {
        p.setObject(idx, serializeTime(v), sqlType)
      }
      override def updateValue(v: OffsetTime, r: ResultSet, idx: Int) = {
        r.updateObject(idx, createPGObject(serializeTime(v), sqlTypeName(None)))
      }
      override def getValue(r: ResultSet, idx: Int): OffsetTime = parseTime(r.getString(idx))
      override val hasLiteralForm : Boolean = false
    }

    class InstantJdbcType extends super.InstantJdbcType with PostgreTimeJdbcType[Instant] {

      private[this] val formatter = {
        new DateTimeFormatterBuilder()
          .append(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss"))
          .optionalStart()
          .appendFraction(ChronoField.NANO_OF_SECOND, 0, 6, true)
          .optionalEnd()
          .optionalStart()
          .appendLiteral("+00")
          .optionalEnd()
          .toFormatter()
      }

      implicit val min : Instant = Instant.MIN
      implicit val max : Instant = Instant.MAX
      implicit val serializeFiniteTime : (Instant => String) =  _.toString
      implicit val parseFiniteTime : (String => Instant) = {
        LocalDateTime.parse(_, formatter).toInstant(ZoneOffset.UTC)
      }

      override val sqlType = java.sql.Types.OTHER
      override def sqlTypeName(sym: Option[FieldSymbol]) = "TIMESTAMP"
      override def getValue(r: ResultSet, idx: Int): Instant = {
        // Postrgres seems to sometimes return strings in the standard UTC timeformat and so Instant.parse
        // works. So try that if there is an initial ParseException
        val str = r.getString(idx)
        try {
          parseTime(str)
        } catch {
          case _: java.time.format.DateTimeParseException => Instant.parse(str)
        }
      }
      override def setValue(v: Instant, p: PreparedStatement, idx: Int) = {
        p.setObject(idx, serializeTime(v), sqlType)
      }

      override def updateValue(v: Instant, r: ResultSet, idx: Int) = {
        r.updateObject(idx, createPGObject(serializeTime(v), sqlTypeName(None)))
      }
      override val hasLiteralForm : Boolean = false
    }

    class LocalDateTimeJdbcType extends super.LocalDateTimeJdbcType with PostgreTimeJdbcType[LocalDateTime] {

      private[this] val formatter = {
        new DateTimeFormatterBuilder()
          .append(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss"))
          .optionalStart()
          .appendFraction(ChronoField.NANO_OF_SECOND,0,6,true)
          .optionalEnd()
          .toFormatter()
      }

      implicit val min : LocalDateTime = LocalDateTime.MIN
      implicit val max : LocalDateTime = LocalDateTime.MAX
      implicit val serializeFiniteTime : (LocalDateTime => String) =  _.format(formatter)
      implicit val parseFiniteTime : (String => LocalDateTime) = LocalDateTime.parse(_, formatter)

      override val sqlType = java.sql.Types.OTHER
      override def sqlTypeName(sym: Option[FieldSymbol]) = "TIMESTAMP"
      override def getValue(r: ResultSet, idx: Int): LocalDateTime = parseTime(r.getString(idx))
      override def setValue(v: LocalDateTime, p: PreparedStatement, idx: Int) = {
        p.setObject(idx, serializeTime(v), sqlType)
      }
      override def updateValue(v: LocalDateTime, r: ResultSet, idx: Int) = {
        r.updateObject(idx, createPGObject(serializeTime(v), sqlTypeName(None)))
      }
      override val hasLiteralForm : Boolean = false
    }

    class UUIDJdbcType extends super.UUIDJdbcType {
      override def sqlTypeName(sym: Option[FieldSymbol]) = "UUID"
      override def setValue(v: UUID, p: PreparedStatement, idx: Int) = p.setObject(idx, v, sqlType)
      override def getValue(r: ResultSet, idx: Int) = r.getObject(idx).asInstanceOf[UUID]
      override def updateValue(v: UUID, r: ResultSet, idx: Int) = r.updateObject(idx, v)
      override def valueToSQLLiteral(value: UUID) = "'" + value + "'"
      override def hasLiteralForm = true
    }
  }
}

object PostgresProfile extends PostgresProfile

// ResultSet.updateObject isn't behaving in the same way as ResultSet.getObject and PreparedStatement.setObject
// when it comes to passing strigified versions of time representations. The error is from the backend
// There will be a "hint: you will need to rewrite or cast the expression" error
// Creating a PGobject and passing the correct type information allows updateObeject to work.
// The postgres jdbc jar isn't on the classpath at compile time, so get access to it with reflection.
object PGUtils {
  val pgObjectClass = Class.forName("org.postgresql.util.PGobject")
  val pgObjectClassCtor = pgObjectClass.getConstructor()
  val pgObjectClassSetType = pgObjectClass.getMethod("setType", classOf[String])
  val pgObjectClassSetValue = pgObjectClass.getMethod("setValue", classOf[String])
  def createPGObject(value: String, dbType: String) = {
    val pgObject = pgObjectClassCtor.newInstance()
    pgObjectClassSetType.invoke(pgObject, dbType)
    pgObjectClassSetValue.invoke(pgObject, value)
    pgObject
  }

}
