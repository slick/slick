package slick.jdbc

import java.sql.{PreparedStatement, ResultSet, Types}
import java.time._
import java.time.format.{DateTimeFormatter, DateTimeFormatterBuilder}
import java.time.temporal.ChronoField

import scala.concurrent.ExecutionContext
import slick.SlickException
import slick.ast._
import slick.basic.Capability
import slick.compiler.{Phase, CompilerState}
import slick.dbio._
import slick.jdbc.meta.MTable
import slick.lifted._
import slick.model.Model
import slick.relational.RelationalProfile
import slick.sql.{SqlProfile, SqlCapabilities}
import slick.util.ConstArray
import slick.util.MacroSupport.macroSupportInterpolation

/** Slick profile for <a href="http://www.hsqldb.org/">HyperSQL</a>
  * (starting with version 2.0).
  *
  * This profile implements the [[slick.jdbc.JdbcProfile]]
  * ''without'' the following capabilities:
  *
  * <ul>
  *   <li>[[slick.sql.SqlCapabilities.sequenceCurr]]:
  *     <code>Sequence.curr</code> to get the current value of a sequence is
  *     not supported by Hsqldb. Trying to generate SQL code which uses this
  *     feature throws a SlickException.</li>
  *   <li>[[slick.jdbc.JdbcCapabilities.insertOrUpdate]]:
  *     InsertOrUpdate operations are emulated on the client side if generated
  *     keys should be returned. Otherwise the operation is performmed
  *     natively on the server side.</li>
  * </ul>
  */
trait HsqldbProfile extends JdbcProfile {

  override protected def computeCapabilities: Set[Capability] = (super.computeCapabilities
    - SqlCapabilities.sequenceCurr
    - JdbcCapabilities.insertOrUpdate
  )

  class ModelBuilder(mTables: Seq[MTable], ignoreInvalidDefaults: Boolean)(implicit ec: ExecutionContext) extends JdbcModelBuilder(mTables, ignoreInvalidDefaults) {
    override def createTableNamer(mTable: MTable): TableNamer = new TableNamer(mTable) {
      override def schema = super.schema.filter(_ != "PUBLIC") // remove default schema
      override def catalog = super.catalog.filter(_ != "PUBLIC") // remove default catalog
    }
  }

  override def createModelBuilder(tables: Seq[MTable], ignoreInvalidDefaults: Boolean)(implicit ec: ExecutionContext): JdbcModelBuilder =
    new ModelBuilder(tables, ignoreInvalidDefaults)

  override def defaultTables(implicit ec: ExecutionContext): DBIO[Seq[MTable]] =
    MTable.getTables(None, None, None, Some(Seq("TABLE")))

  override protected def computeQueryCompiler =
    super.computeQueryCompiler.replace(Phase.resolveZipJoinsRownumStyle) + Phase.specializeParameters - Phase.fixRowNumberOrdering
  override val columnTypes = new JdbcTypes
  override def createQueryBuilder(n: Node, state: CompilerState): QueryBuilder = new QueryBuilder(n, state)
  override def createTableDDLBuilder(table: Table[_]): TableDDLBuilder = new TableDDLBuilder(table)
  override def createSequenceDDLBuilder(seq: Sequence[_]): SequenceDDLBuilder[_] = new SequenceDDLBuilder(seq)

  override protected lazy val useServerSideUpsert = true
  override protected lazy val useServerSideUpsertReturning = false

  override val scalarFrom = Some("(VALUES (0))")

  class QueryBuilder(tree: Node, state: CompilerState) extends super.QueryBuilder(tree, state) {
    override protected val concatOperator = Some("||")
    override protected val alwaysAliasSubqueries = false
    override protected val supportsLiteralGroupBy = true
    override protected val quotedJdbcFns = Some(Nil)

    override def expr(c: Node, skipParens: Boolean = false): Unit = c match {
      case l @ LiteralNode(v: String) if (v ne null) && jdbcTypeFor(l.nodeType).sqlType != Types.CHAR =>
        /* Hsqldb treats string literals as type CHARACTER and pads them with
         * spaces in some expressions, so we cast all string literals to
         * VARCHAR. The length is only 16M instead of 2^31-1 in order to leave
         * enough room for concatenating strings (which extends the size even if
         * it is not needed). */
        b"cast("
        super.expr(c)
        b" as varchar(16777216))"
      /* Hsqldb uses the SQL:2008 syntax for NEXTVAL */
      case Library.NextValue(SequenceNode(name)) => b"(next value for `$name)"
      case Library.CurrentValue(_*) => throw new SlickException("Hsqldb does not support CURRVAL")
      case RowNumber(_) => b"rownum()" // Hsqldb uses Oracle ROWNUM semantics but needs parens
      case _ => super.expr(c, skipParens)
    }

    override protected def buildJoin(j: Join): Unit = {
      /* Re-balance inner joins to the left because Hsqldb does not supported RHS nesting. Paths
       * into joined views have already been mapped to unique identifiers at this point, so we can
       * safely rearrange views. */
      j match {
        case Join(ls, rs, l, Join(ls2, rs2, l2, r2, JoinType.Inner, on2), JoinType.Inner, on) =>
          val on3 = (on, on2) match {
            case (a, LiteralNode(true)) => a
            case (LiteralNode(true), b) => b
            case (a, b) => Apply(Library.And, ConstArray(a, b))(UnassignedType)
          }
          buildJoin(Join(rs, rs2, Join(ls, ls2, l, l2, JoinType.Inner, LiteralNode(true)), r2, JoinType.Inner, on3))
        case j => super.buildJoin(j)
      }
    }

    override protected def buildFetchOffsetClause(fetch: Option[Node], offset: Option[Node]) = (fetch, offset) match {
      case (Some(t), Some(d)) => b"\nlimit $t offset $d"
      case (Some(t), None   ) => b"\nlimit $t"
      case (None, Some(d)   ) => b"\noffset $d"
      case _ =>
    }
  }

  class JdbcTypes extends super.JdbcTypes {
    override val byteArrayJdbcType = new ByteArrayJdbcType {
      override def sqlTypeName(sym: Option[FieldSymbol]) = "LONGVARBINARY"
    }
    override val uuidJdbcType = new UUIDJdbcType {
      override def sqlType = java.sql.Types.BINARY

      override def sqlTypeName(sym: Option[FieldSymbol]) = "BINARY(16)"
    }
    override val localTimeType = new LocalTimeJdbcType {
      override def sqlType = java.sql.Types.TIME

      override def sqlTypeName(sym: Option[FieldSymbol]) = "TIME(3)"

      override def getValue(r: ResultSet, idx: Int): LocalTime = {
        // Fixes an issue caused because Hsqldb outputs the time in a non-standard format
        // not adding trailing zeros to the hours. For example: '2:14:41.421' instead of '02:14:41.421'
        r.getString(idx) match {
          case null => null
          case isoTime if isoTime.indexOf(':') == 2 => LocalTime.parse(isoTime)
          case isoTime if isoTime.indexOf(':') == 1 => LocalTime.parse(s"0$isoTime")
          case isoTime => throw new RuntimeException(s"$isoTime is not a valid Hsqldb String")
        }
      }
    }

    override val offsetTimeType = new OffsetTimeJdbcType
    override val offsetDateTimeType = new OffsetDateTimeJdbcType

    trait HsqldbTimeJdbcTypeWithOffset {

      /**
        * Finds the position of the problematic character during serialization / deserialization.
        */
      @inline
      private[this] def offsetTypeCharPosition(isoString : String): Int = {
        isoString.lastIndexWhere(x => x == '+' || x == '-' || x == 'Z')
      }

      /**
        * Finds the offset char from a ISO-8601 [[String]]
        */
      @inline
      private[this] def offsetTypeChar(isoString : String) : Char = {
        isoString.charAt(offsetTypeCharPosition(isoString))
      }

      /**
        * Fix issue caused because Hsqldb works with the offset and the hour in a non
        * standard format. For example: '2:14:41.421+1:00' instead of '02:14:41.421+01:00'
        */
      def serializeIsoTime(isoString: String): String = {
        if (isoString == null) {
          "NULL"
        } else {
          offsetTypeChar(isoString) match {
            case 'Z' =>
              isoString.dropRight(1).concat("+0:00")
            case _ =>
              val problematicCharacterIndex: Int = offsetTypeCharPosition(isoString) + 1
              problematicCharacterIndex match {
                case index if isoString.charAt(index) == '0' =>
                  // We need to take out this character from the ISO-8601 [[String]
                  @inline val timestamp: String = isoString.substring(0, index)
                  @inline val offset: String = isoString.substring(index + 1)
                  s"$timestamp$offset"
                case _ =>
                  isoString
              }
          }
        }
      }

      /**
        * Fix issue caused because Hsqldb outputs the offset and the hours in a non standard
        * format. For example: '2:14:41.421+1:00' instead of '02:14:41.421+01:00' (The hours and
        * the offset do not have a trailing zero)
        */
      def parseHsqldbTime (hsqldbString: String) : String = {
        hsqldbString match {
          case null => null
          case _ =>
            @inline val normalizedTimeFormatString : String = normalizeTimeFormat(hsqldbString)
            normalizeOffset(normalizedTimeFormatString)
        }
      }

      @inline
      private[this] def normalizeTimeFormat (hsqldbString : String) : String = {
        hsqldbString.indexOf(':') match {
          case 2 =>
            hsqldbString
          case 1 =>
            s"0$hsqldbString"
          case index if hsqldbString.charAt(index - 2) == ' ' =>
            s"${hsqldbString.substring(0, index - 1)}0${hsqldbString.substring(index)}"
          case index if hsqldbString.charAt(index - 2).asDigit >= 0 && hsqldbString.charAt(index - 2).asDigit <= 9 =>
            hsqldbString
          case _ =>
            throw new RuntimeException(s"$hsqldbString do not have a valid Hsqldb format")
        }
      }

      @inline
      private[this] def normalizeOffset(isoString: String): String = {
        isoString match {
          case _ if isoString.endsWith("+0:00") =>
            isoString.dropRight(5).concat("Z")
          case _ =>
            offsetTypeCharPosition(isoString) match {
              case index if isoString.length - index - 1 == 5 => // ends with format -> +HH:MM
                isoString
              case index =>
                // We add a missing '0' character in order to accomplish the ISO-8601 time format.
                @inline val timestamp: String = isoString.substring(0, index + 1)
                @inline val offset: String = isoString.substring(index + 1)
                s"${timestamp}0$offset"
            }
        }
      }
    }

    class OffsetTimeJdbcType extends super.OffsetTimeJdbcType with HsqldbTimeJdbcTypeWithOffset {

      override def sqlType = java.sql.Types.OTHER

      override def sqlTypeName(sym: Option[FieldSymbol]) = "TIME(3) WITH TIME ZONE"

      override val hasLiteralForm: Boolean = false

      override def setValue(v: OffsetTime, p: PreparedStatement, idx: Int) = {
        @inline val fixedOffsetTime: String = serializeIsoTime(v.toString)
        p.setString(idx, fixedOffsetTime)
      }

      override def updateValue(v: OffsetTime, r: ResultSet, idx: Int) = {
        @inline val fixedOffsetTime: String = serializeIsoTime(v.toString)
        r.updateString(idx, fixedOffsetTime)
      }

      override def getValue(r: ResultSet, idx: Int): OffsetTime = {
        r.getString(idx) match {
          case null => null
          case hsqldbString =>
            @inline val normalizedIsoString : String = parseHsqldbTime(hsqldbString)
            OffsetTime.parse(normalizedIsoString)
        }
      }
    }

    class OffsetDateTimeJdbcType extends super.OffsetDateTimeJdbcType with HsqldbTimeJdbcTypeWithOffset {

      private[this] val formatter: DateTimeFormatter = {
        new DateTimeFormatterBuilder()
          .append(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss"))
          .optionalStart()
          .appendFraction(ChronoField.NANO_OF_SECOND, 0, 3, true)
          .optionalEnd()
          .appendOffset("+HH:MM", "Z")
          .toFormatter()
      }

      override def sqlType = java.sql.Types.OTHER

      override def sqlTypeName(sym: Option[FieldSymbol]) = "TIMESTAMP(3) WITH TIME ZONE"

      override val hasLiteralForm: Boolean = false

      override def setValue(v: OffsetDateTime, p: PreparedStatement, idx: Int) = {
        @inline val fixedOffsetTime: String = serializeIsoTime(v.format(formatter))
        p.setString(idx, fixedOffsetTime)
      }

      override def updateValue(v: OffsetDateTime, r: ResultSet, idx: Int) = {
        @inline val fixedOffsetTime: String = serializeIsoTime(v.format(formatter))
        r.updateString(idx, fixedOffsetTime)
      }

      override def getValue(r: ResultSet, idx: Int): OffsetDateTime = {
        r.getString(idx) match {
          case null => null
          case hsqldbString =>
            @inline val normalizedIsoString: String = parseHsqldbTime(hsqldbString)
            OffsetDateTime.parse(normalizedIsoString, formatter)
        }
      }
    }
  }

  class TableDDLBuilder(table: Table[_]) extends super.TableDDLBuilder(table) {
    override protected def createIndex(idx: Index) = {
      if(idx.unique) {
        /* Create a UNIQUE CONSTRAINT (with an automatically generated backing
         * index) because Hsqldb does not allow a FOREIGN KEY CONSTRAINT to
         * reference columns which have a UNIQUE INDEX but not a nominal UNIQUE
         * CONSTRAINT. */
        val sb = new StringBuilder append "ALTER TABLE " append quoteIdentifier(table.tableName) append " ADD "
        sb append "CONSTRAINT " append quoteIdentifier(idx.name) append " UNIQUE("
        addIndexColumnList(idx.on, sb, idx.table.tableName)
        sb append ")"
        sb.toString
      } else super.createIndex(idx)
    }
  }

  class SequenceDDLBuilder[T](seq: Sequence[T]) extends super.SequenceDDLBuilder(seq) {
    override def buildDDL: DDL = {
      import seq.integral._
      val increment = seq._increment.getOrElse(one)
      val desc = increment < zero
      val start = seq._start.getOrElse(if(desc) -1 else 1)
      val b = new StringBuilder append "CREATE SEQUENCE " append quoteIdentifier(seq.name)
      seq._increment.foreach { b append " INCREMENT BY " append _ }
      seq._minValue.foreach { b append " MINVALUE " append _ }
      seq._maxValue.foreach { b append " MAXVALUE " append _ }
      /* The START value in Hsqldb defaults to 0 instead of the more
       * conventional 1/-1 so we rewrite it to make 1/-1 the default. */
      if(start != 0) b append " START WITH " append start
      if(seq._cycle) b append " CYCLE"
      DDL(b.toString, "DROP SEQUENCE " + quoteIdentifier(seq.name))
    }
  }
}

object HsqldbProfile extends HsqldbProfile
