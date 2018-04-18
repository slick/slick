
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
import slick.sql.SqlCapabilities
import slick.relational.RelationalProfile
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

  override def defaultSqlTypeName(tmd: JdbcType[_], sym: Option[FieldSymbol]): String = tmd.sqlType match {
    case java.sql.Types.VARCHAR =>
      val size = sym.flatMap(_.findColumnOption[RelationalProfile.ColumnOption.Length])
      size.fold("LONGVARCHAR")(l => if(l.varying) s"VARCHAR(${l.length})" else s"CHAR(${l.length})")
    case _ => super.defaultSqlTypeName(tmd, sym)
  }

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
        // not adding leading zeros to the hours. For example: '2:14:41.421' instead of '02:14:41.421'
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
    override val instantType = new InstantJdbcType


    /**
      * HSQLDB uses a non-standard string representation of timestamps. It doesn't pad the hour and offset-hour with zeros.
      * Although the hour can be handled by DateTimeFormatterBuilder, the offset hour can't. See it's method appendOffset()
      * So we handle this in two steps: first pad (trim) zeros in the offset, and then use a custom DateTimeFormatterBuilder.
      */
    trait HsqldbTimeJdbcTypeWithOffset {
      protected val timeFormatter: DateTimeFormatter = {
        new DateTimeFormatterBuilder()
          .append(DateTimeFormatter.ofPattern("H:mm:ss"))
          .optionalStart()
          .appendFraction(ChronoField.NANO_OF_SECOND, 0, 9, true)
          .optionalEnd()
          .appendOffset("+HH:MM", "Z")
          .toFormatter()
      }

      protected val datetimeFormatter = {
        new DateTimeFormatterBuilder()
          .append(DateTimeFormatter.ISO_LOCAL_DATE)
          .appendLiteral(' ')
          .append(timeFormatter)
          .toFormatter()
      }

      /**
        * Add offset padding e.g. +1:00 becomes +01:00.
        */
      def offsetConvertHsqldbToISO(hsqldbString : String) : String = hsqldbString.takeRight(5).toArray match {
        case Array('+'|'-',_,':',_,_) => hsqldbString.dropRight(4) + "0" + hsqldbString.takeRight(4)
        case _ => hsqldbString
      }

      /**
        * Remove offset padding e.g. +01:00 becomes +1:00. 'Z' becomes '+0:00'.
        */
      def offsetConvertISOToHsqldb(isoString : String) : String = isoString.takeRight(6).toArray match {
        case Array(_,_,_,_,_,'Z') => isoString.dropRight(1) + "+0:00"
        case Array('+'|'-','0',_,':',_,_) => isoString.dropRight(5) + isoString.takeRight(4)
        case _ => isoString
      }
    }

    class OffsetTimeJdbcType extends super.OffsetTimeJdbcType with HsqldbTimeJdbcTypeWithOffset {

      override def sqlType = java.sql.Types.TIME_WITH_TIMEZONE

      override def sqlTypeName(sym: Option[FieldSymbol]) = "TIME(9) WITH TIME ZONE"

      override val hasLiteralForm: Boolean = false

      override def setValue(v: OffsetTime, p: PreparedStatement, idx: Int) = {
        @inline val correctedOffsetTime: String = offsetConvertISOToHsqldb(v.format(timeFormatter))
        p.setString(idx, correctedOffsetTime)
      }

      override def updateValue(v: OffsetTime, r: ResultSet, idx: Int) = {
        @inline val correctedOffsetTime: String = offsetConvertISOToHsqldb(v.format(timeFormatter))
        r.updateString(idx, correctedOffsetTime)
      }

      override def getValue(r: ResultSet, idx: Int): OffsetTime = {
        r.getString(idx) match {
          case null => null
          case hsqldbString =>
            @inline val normalizedIsoString : String = offsetConvertHsqldbToISO(hsqldbString)
            // in a subsecond greater than 00:00:00 and with an offset, we seem to get back a string like
            // '24:00:00.745+01:00' instead of '00:00:00.745+01:00' which is an invalid OffsetTime,
            // so update the leading 24 to 00
            OffsetTime.parse(
              if (normalizedIsoString.startsWith("24:00:00"))
                "00" + normalizedIsoString.substring(2)
              else
                normalizedIsoString, timeFormatter)
        }
      }
    }

    class OffsetDateTimeJdbcType extends super.OffsetDateTimeJdbcType with HsqldbTimeJdbcTypeWithOffset {

      override def sqlType = java.sql.Types.TIMESTAMP_WITH_TIMEZONE

      override def sqlTypeName(sym: Option[FieldSymbol]) = "TIMESTAMP(9) WITH TIME ZONE"

      override val hasLiteralForm: Boolean = false

      override def setValue(v: OffsetDateTime, p: PreparedStatement, idx: Int) = {
        @inline val correctedOffsetTime: String = offsetConvertISOToHsqldb(v.format(datetimeFormatter))
        p.setString(idx, correctedOffsetTime)
      }

      override def updateValue(v: OffsetDateTime, r: ResultSet, idx: Int) = {
        @inline val correctedOffsetTime: String = offsetConvertISOToHsqldb(v.format(datetimeFormatter))
        r.updateString(idx, correctedOffsetTime)
      }

      override def getValue(r: ResultSet, idx: Int): OffsetDateTime = {
        r.getString(idx) match {
          case null => null
          case hsqldbString =>
            @inline val normalizedIsoString: String = offsetConvertHsqldbToISO(hsqldbString)
            OffsetDateTime.parse(normalizedIsoString,datetimeFormatter)
        }
      }
    }
    class InstantJdbcType extends super.InstantJdbcType with HsqldbTimeJdbcTypeWithOffset {
      override def sqlType = java.sql.Types.TIMESTAMP_WITH_TIMEZONE
      override def sqlTypeName(sym: Option[FieldSymbol]) = "TIMESTAMP(9) WITH TIME ZONE"
      override val hasLiteralForm: Boolean = false
      override def setValue(v: Instant, p: PreparedStatement, idx: Int) = {
        @inline val correctedOffsetTime = offsetConvertISOToHsqldb(
          OffsetDateTime.ofInstant(v, ZoneOffset.UTC).format(datetimeFormatter))
        p.setString(idx, correctedOffsetTime)
      }

      override def updateValue(v: Instant, r: ResultSet, idx: Int) = {
        @inline val correctedOffsetTime: String = offsetConvertISOToHsqldb(
          OffsetDateTime.ofInstant(v, ZoneOffset.UTC).format(datetimeFormatter))
        r.updateString(idx, correctedOffsetTime)
      }

      override def getValue(r: ResultSet, idx: Int): Instant = {
        r.getString(idx) match {
          case null => null
          case hsqldbString =>
            @inline val normalizedIsoString: String = offsetConvertHsqldbToISO(hsqldbString)
            OffsetDateTime.parse(normalizedIsoString,datetimeFormatter).toInstant
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
