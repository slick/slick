package slick.jdbc

import java.sql.{PreparedStatement, ResultSet}
import java.time.*
import java.time.format.{DateTimeFormatter, DateTimeFormatterBuilder}
import java.time.temporal.ChronoField
import java.util.UUID

import scala.concurrent.ExecutionContext

import slick.ast.*
import slick.basic.Capability
import slick.compiler.{CompilerState, Phase}
import slick.dbio.*
import slick.jdbc.meta.{MColumn, MIndexInfo, MTable}
import slick.relational.RelationalProfile
import slick.util.ConstArray
import slick.util.QueryInterpolator.queryInterpolator

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
trait PostgresProfile extends JdbcProfile with JdbcActionComponent.MultipleRowsPerStatementSupport {

  override protected def computeCapabilities: Set[Capability] = super.computeCapabilities
    - JdbcCapabilities.insertOrUpdate
    - JdbcCapabilities.insertOrUpdateWithPrimaryKeyOnly
    - JdbcCapabilities.nullableNoDefault
    - JdbcCapabilities.supportsByte

  class ModelBuilder(mTables: Seq[MTable], ignoreInvalidDefaults: Boolean)(implicit ec: ExecutionContext)
    extends JdbcModelBuilder(mTables, ignoreInvalidDefaults) {

    override def createTableNamer(mTable: MTable): TableNamer = new PostgresTableNamer(mTable)
    override def createColumnBuilder(tableBuilder: TableBuilder, meta: MColumn): ColumnBuilder =
      new PostgresColumnBuilder(tableBuilder, meta)
    override def createIndexBuilder(tableBuilder: TableBuilder, meta: Seq[MIndexInfo]): IndexBuilder =
      new PostgresIndexBuilder(tableBuilder, meta)

    class PostgresTableNamer(mTable: MTable) extends TableNamer(mTable) {
      override def schema = super.schema.filter(_ != "public") // remove default schema
    }
    class PostgresColumnBuilder(tableBuilder: TableBuilder, meta: MColumn) extends ColumnBuilder(tableBuilder, meta) {
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
      override def default = meta.columnDef.map((_, tpe)).collect {
        case ("true", "Boolean")                          => Some(Some(true))
        case ("false", "Boolean")                         => Some(Some(false))
        case (TextPattern(str), "String")                 => Some(Some(str))
        case ("NULL::bpchar", "String")                   => Some(None)
        case (TextPattern(str), "Char")                   => str.length match {
          case 0 => Some(Some(' ')) // Default to one space, as the char will be space padded anyway
          case 1 => Some(Some(str.head))
          case _ => None // This is invalid, so let's not supply any default
        }
        case ("NULL::bpchar", "Char")                     => Some(None)
        case (NumericPattern(v), "Short")                 => Some(Some(v.toShort))
        case (NumericPattern(v), "Int")                   => Some(Some(v.toInt))
        case (NumericPattern(v), "Long")                  => Some(Some(v.toLong))
        case (NumericPattern(v), "Float")                 => Some(Some(v.toFloat))
        case (NumericPattern(v), "Double")                => Some(Some(v.toDouble))
        case (NumericPattern(v), "scala.math.BigDecimal") => Some(Some(BigDecimal(s"$v")))
        case (UUIDPattern(v), "java.util.UUID")           => Some(Some(java.util.UUID.fromString(v)))
        // The UUID is generated through a function - treat it as if there was no default.
        case (_, "java.util.UUID") => None
      }.getOrElse {
        val d = super.default
        if (meta.nullable.contains(true) && d.isEmpty) {
          Some(None)
        } else d
      }
      override def varying: Boolean =
        dbType.contains("citext") || super.varying
      override def length: Option[Int] = {
        val l = super.length
        if(tpe == "String" && varying && l.contains(2147483647)) None
        else l
      }
      override def tpe = meta.typeName match {
        case "bytea" => "Array[Byte]"
        case "lo" if meta.sqlType == java.sql.Types.DISTINCT => "java.sql.Blob"
        case "uuid" => "java.util.UUID"
        case "citext" => "String"
        case _ => super.tpe
      }
    }
    class PostgresIndexBuilder(tableBuilder: TableBuilder, meta: Seq[MIndexInfo])
      extends IndexBuilder(tableBuilder, meta) {
      // FIXME: this needs a test
      override def columns = super.columns.map(_.stripPrefix("\"").stripSuffix("\""))
    }
  }

  override def createModelBuilder(tables: Seq[MTable], ignoreInvalidDefaults: Boolean)
                                 (implicit ec: ExecutionContext): JdbcModelBuilder =
    new ModelBuilder(tables, ignoreInvalidDefaults)

  override def defaultTables(implicit ec: ExecutionContext): DBIO[Seq[MTable]] =
    MTable.getTables(None, None, Some("%"), Some(Seq("TABLE")))

  override val columnTypes: PostgresJdbcTypes = new PostgresJdbcTypes
  override protected def computeQueryCompiler = super.computeQueryCompiler - Phase.rewriteDistinct
  override def createQueryBuilder(n: Node, state: CompilerState): PostgresQueryBuilder =
    new PostgresQueryBuilder(n, state)
  override def createUpsertBuilder(node: Insert): PostgresUpsertBuilder = new PostgresUpsertBuilder(node)
  override def createTableDDLBuilder(table: Table[?]): PostgresTableDDLBuilder = new PostgresTableDDLBuilder(table)
  override def createColumnDDLBuilder(column: FieldSymbol, table: Table[?]): PostgresColumnDDLBuilder =
    new PostgresColumnDDLBuilder(column)
  override protected lazy val useServerSideUpsert = true
  override protected lazy val useTransactionForUpsert = true
  override protected lazy val useServerSideUpsertReturning = false

  override def defaultSqlTypeName(tmd: JdbcType[?], sym: Option[FieldSymbol]): String = tmd.sqlType match {
    case java.sql.Types.VARCHAR =>
      val size = sym.flatMap(_.findColumnOption[RelationalProfile.ColumnOption.Length])
      size.fold("VARCHAR")(l => if(l.varying) s"VARCHAR(${l.length})" else s"CHAR(${l.length})")
    case java.sql.Types.BLOB => "lo"
    case java.sql.Types.DOUBLE => "DOUBLE PRECISION"
    /* PostgreSQL does not have a TINYINT type, so we use SMALLINT instead. */
    case java.sql.Types.TINYINT => "SMALLINT"
    case _ => super.defaultSqlTypeName(tmd, sym)
  }

  class PostgresQueryBuilder(tree: Node, state: CompilerState) extends QueryBuilder(tree, state) {
    override protected val concatOperator: Some[String] = Some("||")
    override protected val quotedJdbcFns: Some[Vector[Library.JdbcFunction]] =
      Some(Vector(Library.Database, Library.User))

    override protected def buildSelectModifiers(c: Comprehension.Base): Unit = (c.distinct, c.select) match {
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

    override def expr(n: Node) = n match {
      case Library.UCase(ch) => b"upper($ch)"
      case Library.LCase(ch) => b"lower($ch)"
      case Library.IfNull(ch, d) => b"coalesce($ch, $d)"
      case Library.NextValue(SequenceNode(name)) => b"nextval('$name')"
      case Library.CurrentValue(SequenceNode(name)) => b"currval('$name')"
      case Library.CurrentDate() => b"current_date"
      case Library.CurrentTime() => b"current_time"
      case Union(left, right, all) =>
        b"\{"
        buildFrom(left, None, false)
        if (all) b"\nunion all " else b"\nunion "
        buildFrom(right, None, false)
        b"\}"
      case _ => super.expr(n)
    }
  }

  class PostgresUpsertBuilder(ins: Insert) extends UpsertBuilder(ins) {
    override def buildInsert: InsertBuilderResult = {
      val update =
        "update " +
          tableName +
          " set " +
          softNames.map(n => s"$n=?").mkString(",") +
          " where " +
          pkNames.map(n => s"$n=?").mkString(" and ")
      val nonAutoIncNames = nonAutoIncSyms.map(fs => quoteIdentifier(fs.name)).mkString(",")
      val nonAutoIncVars = nonAutoIncSyms.map(_ => "?").mkString(",")
      val cond = pkNames.map(n => s"$n=?").mkString(" and ")
      val insert =
        s"insert into $tableName ($nonAutoIncNames)" +
          s" select $nonAutoIncVars where not exists (select 1 from $tableName where $cond)"
      new InsertBuilderResult(table, s"$update; $insert", ConstArray.from(softSyms ++ pkSyms))
    }

    override def transformMapping(n: Node) = reorderColumns(n, softSyms ++ pkSyms ++ nonAutoIncSyms.toSeq ++ pkSyms)
  }

  class PostgresTableDDLBuilder(table: Table[?]) extends TableDDLBuilder(table) {
    override def createPhase1 = super.createPhase1 ++ columns.flatMap {
      case cb: PostgresColumnDDLBuilder => cb.createLobTrigger(table.tableName)
    }
    override def dropPhase1 = {
      val dropLobs = columns.flatMap {
        case cb: PostgresColumnDDLBuilder => cb.dropLobTrigger(table.tableName)
      }
      if(dropLobs.isEmpty) super.dropPhase1
      else Seq("delete from "+quoteIdentifier(table.tableName)) ++ dropLobs ++ super.dropPhase1
    }
  }

  class PostgresColumnDDLBuilder(column: FieldSymbol) extends ColumnDDLBuilder(column) {
    override protected def appendOptions(sb: StringBuilder): Unit = {
      if(defaultLiteral ne null) sb append " DEFAULT " append defaultLiteral
      if(notNull) sb append " NOT NULL"
      if(primaryKey) sb append " PRIMARY KEY"
      if(unique) sb append " UNIQUE"
    }
    override def appendColumn(sb: StringBuilder): Unit = {
      sb append quoteIdentifier(column.name) append ' '
      if(autoIncrement && !customSqlType) {
        sb append (if(sqlType.toUpperCase == "BIGINT") "BIGSERIAL" else "SERIAL")
      } else appendType(sb)
      appendOptions(sb)
    }

    def lobTrigger(name: String) =
      quoteIdentifier(name+"__"+quoteIdentifier(column.name)+"_lob")

    def createLobTrigger(name: String): Option[String] =
      if(sqlType == "lo") Some(
        "create trigger "+lobTrigger(name)+" before update or delete on "+
        quoteIdentifier(name)+" for each row execute procedure lo_manage("+quoteIdentifier(column.name)+")"
      ) else None

    def dropLobTrigger(name: String): Option[String] =
      if(sqlType == "lo") Some(
        "drop trigger "+lobTrigger(name)+" on "+quoteIdentifier(name)
      ) else None
  }

  class PostgresJdbcTypes extends JdbcTypes {
    override val byteArrayJdbcType: PostgresByteArrayJdbcType = new PostgresByteArrayJdbcType
    override val uuidJdbcType: PostgresUUIDJdbcType = new PostgresUUIDJdbcType
    override val localDateType: PostgresLocalDateJdbcType = new PostgresLocalDateJdbcType
    override val localTimeType: PostgresLocalTimeJdbcType = new PostgresLocalTimeJdbcType
    override val offsetTimeType: PostgresOffsetTimeJdbcType = new PostgresOffsetTimeJdbcType
    //OffsetDateTime and ZonedDateTime not currently supportable natively by the backend
    override val instantType: PostgresInstantJdbcType = new PostgresInstantJdbcType
    override val localDateTimeType: PostgresLocalDateTimeJdbcType = new PostgresLocalDateTimeJdbcType

    class PostgresByteArrayJdbcType extends ByteArrayJdbcType {
      override val sqlType = java.sql.Types.BINARY
      override def sqlTypeName(sym: Option[FieldSymbol]) = "BYTEA"
    }

    trait PostgresTimeJdbcType [T] {

      val min : T
      val max : T
      val serializeFiniteTime : T => String
      val parseFiniteTime : String => T

      protected def serializeTime(time : T): String = {
        time match {
          case null => null
          case _ => serializeFiniteTime(time)
        }
      }

      protected def parseTime(time : String): T = {
        time match {
          case null => null.asInstanceOf[T]
          case _ => parseFiniteTime(time)
        }
      }
    }

    trait PostgresInfinityTimeJdbcType [T] extends PostgresTimeJdbcType[T] {

      @inline
      private[this] val negativeInfinite = "-infinity"
      @inline
      private[this] val positiveInfinite = "infinity"

      override protected def serializeTime(time : T): String = {
        time match {
          case null => null
          case `min` => negativeInfinite
          case `max` => positiveInfinite
          case _ => serializeFiniteTime(time)
        }
      }

      override protected def parseTime(time : String): T = {
        time match {
          case null => null.asInstanceOf[T]
          case `negativeInfinite` => min
          case `positiveInfinite` => max
          case _ => parseFiniteTime(time)
        }
      }
    }

    import PGUtils.createPGObject
    class PostgresLocalDateJdbcType extends LocalDateJdbcType with PostgresInfinityTimeJdbcType[LocalDate] {

      private[this] val formatter = DateTimeFormatter.ISO_LOCAL_DATE

      val min : LocalDate = LocalDate.MIN
      val max : LocalDate = LocalDate.MAX
      val serializeFiniteTime : LocalDate => String =  _.format(formatter)
      val parseFiniteTime : String => LocalDate = LocalDate.parse(_, formatter)

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

    class PostgresLocalTimeJdbcType extends LocalTimeJdbcType with PostgresTimeJdbcType[LocalTime] {

      private[this] val formatter : DateTimeFormatter = DateTimeFormatter.ISO_LOCAL_TIME

      val min : LocalTime = LocalTime.MIN
      val max : LocalTime = LocalTime.MAX
      val serializeFiniteTime : LocalTime => String =  _.format(formatter)
      val parseFiniteTime : String => LocalTime = {
        case "24:00:00" => LocalTime.MAX
        case value      => LocalTime.parse(value, formatter)
      }

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

    class PostgresOffsetTimeJdbcType extends OffsetTimeJdbcType with PostgresTimeJdbcType[OffsetTime] {

      private[this] val formatter : DateTimeFormatter = {
        new DateTimeFormatterBuilder()
          .append(DateTimeFormatter.ofPattern("HH:mm:ss"))
          .optionalStart()
          .appendFraction(ChronoField.NANO_OF_SECOND, 0, 6, true)
          .optionalEnd()
          .appendOffset("+HH:mm", "+00")
          .toFormatter()
      }

      // Postgres max time zone +1559
      private[this] val maxPostgresTimeZone : ZoneOffset = ZoneOffset.ofHoursMinutes(15, 59)

      // Postgres min time zone -1559
      private[this] val minPostgresTimeZone : ZoneOffset = ZoneOffset.ofHoursMinutes(-15, -59)

      val min : OffsetTime = OffsetTime.MIN
      val max : OffsetTime = OffsetTime.MAX
      val serializeFiniteTime : OffsetTime => String = offsetTime => {
        val adjusted = offsetTime.getOffset.getTotalSeconds match {
          case offset if offset > maxPostgresTimeZone.getTotalSeconds =>
            offsetTime.withOffsetSameLocal(maxPostgresTimeZone)
          case offset if offset < minPostgresTimeZone.getTotalSeconds =>
            offsetTime.withOffsetSameLocal(minPostgresTimeZone)
          case _                                                      => offsetTime
        }

        adjusted.format(formatter)
      }
      val parseFiniteTime : String => OffsetTime = OffsetTime.parse(_, formatter)

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

    class PostgresInstantJdbcType extends InstantJdbcType with PostgresInfinityTimeJdbcType[Instant] {
      private[this] val formatter = {
        new DateTimeFormatterBuilder()
          .append(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss"))
          .optionalStart()
          .appendFraction(ChronoField.NANO_OF_SECOND, 0, 6, true)
          .optionalEnd()
          .optionalStart()
          .appendOffset("+HH:mm", "+00")
          .optionalEnd()
          .toFormatter()
      }

      val min : Instant = Instant.MIN
      val max : Instant = Instant.MAX
      val serializeFiniteTime : Instant => String =  _.toString
      val parseFiniteTime : String => Instant = { s =>
        val parsed = formatter.parse(s)
        if (parsed.isSupported(ChronoField.INSTANT_SECONDS)) {
          Instant.from(parsed)
        } else {
          LocalDateTime.from(parsed).toInstant(ZoneOffset.UTC)
        }
      }

      override val sqlType = java.sql.Types.OTHER
      override def sqlTypeName(sym: Option[FieldSymbol]) = "TIMESTAMP"
      override def getValue(r: ResultSet, idx: Int): Instant = {
        // Postgres seems to sometimes return strings in the standard UTC time format and so Instant.parse
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

    class PostgresLocalDateTimeJdbcType extends LocalDateTimeJdbcType with PostgresInfinityTimeJdbcType[LocalDateTime] {
      private[this] val formatter = {
        new DateTimeFormatterBuilder()
          .append(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss"))
          .optionalStart()
          .appendFraction(ChronoField.NANO_OF_SECOND,0,6,true)
          .optionalEnd()
          .toFormatter()
      }

      val min : LocalDateTime = LocalDateTime.MIN
      val max : LocalDateTime = LocalDateTime.MAX
      val serializeFiniteTime : LocalDateTime => String =  _.format(formatter)
      val parseFiniteTime : String => LocalDateTime = LocalDateTime.parse(_, formatter)

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

    class PostgresUUIDJdbcType extends UUIDJdbcType {
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
// when it comes to passing stringified versions of time representations. The error is from the backend
// There will be a "hint: you will need to rewrite or cast the expression" error
// Creating a PGobject and passing the correct type information allows updateObject to work.
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
