package slick.jdbc

import java.sql.{PreparedStatement, ResultSet}
import java.time._
import java.time.format.{DateTimeFormatter, DateTimeFormatterBuilder}
import java.time.temporal.ChronoField

import scala.concurrent.ExecutionContext
import java.util.{TimeZone, UUID}

import slick.basic.Capability
import slick.relational.{RelationalCapabilities, RelationalProfile}
import slick.sql.SqlCapabilities
import slick.ast._
import slick.util.MacroSupport.macroSupportInterpolation
import slick.compiler.{CompilerState, Phase}
import slick.jdbc.meta.{MColumn, MTable}

/** Slick profile for H2.
  *
  * This profile implements [[slick.jdbc.JdbcProfile]]
  * ''without'' the following capabilities:
  *
  * <ul>
  *   <li>[[slick.relational.RelationalCapabilities.reverse]]:
  *     This String function is not available in H2.</li>
  *   <li>[[slick.sql.SqlCapabilities.sequenceMin]],
  *     [[slick.sql.SqlCapabilities.sequenceMax]],
  *     [[slick.sql.SqlCapabilities.sequenceCycle]]:
  *     H2 does not support MINVALUE, MAXVALUE and CYCLE</li>
  *   <li>[[slick.jdbc.JdbcCapabilities.returnInsertOther]]:
  *     When returning columns from an INSERT operation, only a single column
  *     may be specified which must be the table's AutoInc column.</li>
  *   <li>[[slick.relational.RelationalCapabilities.joinFull]]:
  *     Full outer joins are emulated because there is not native support
  *     for them.</li>
  *   <li>[[slick.jdbc.JdbcCapabilities.insertOrUpdate]]:
  *     InsertOrUpdate operations are emulated on the client side if the
  *     data to insert contains an `AutoInc` fields. Otherwise the operation
  *     is performmed natively on the server side.</li>
  * </ul>
  */
trait H2Profile extends JdbcProfile {

  override protected def computeCapabilities: Set[Capability] = (super.computeCapabilities
    - SqlCapabilities.sequenceMin
    - SqlCapabilities.sequenceMax
    - SqlCapabilities.sequenceCycle
    - JdbcCapabilities.returnInsertOther
    - RelationalCapabilities.joinFull
    - JdbcCapabilities.insertOrUpdate
    - RelationalCapabilities.reverse
  )

  class ModelBuilder(mTables: Seq[MTable], ignoreInvalidDefaults: Boolean)(implicit ec: ExecutionContext) extends JdbcModelBuilder(mTables, ignoreInvalidDefaults) {
    override def createTableNamer(mTable: MTable): TableNamer = new TableNamer(mTable) {
      override def schema = super.schema.filter(_ != "PUBLIC") // remove default schema
    }
    override def createColumnBuilder(tableBuilder: TableBuilder, meta: MColumn): ColumnBuilder = new ColumnBuilder(tableBuilder, meta) {
      override def length = super.length.filter(_ != Int.MaxValue) // H2 sometimes show this value, but doesn't accept it back in the DBType
      override def default = rawDefault.map((_,tpe)).collect{
          case (v,"java.util.UUID") =>
            if (v.matches("^['\"].*['\"]$"))
              Some(Some(java.util.UUID.fromString(v.replaceAll("[\'\"]", "")))) //strip quotes
            else
              None // The UUID is generated through a function - treat it as if there was no default.
        }.getOrElse{super.default}
      override def tpe = dbType match {
        case Some("UUID") => "java.util.UUID"
        case _ => super.tpe
      }
    }
  }

  override def createModelBuilder(tables: Seq[MTable], ignoreInvalidDefaults: Boolean)(implicit ec: ExecutionContext): JdbcModelBuilder =
    new ModelBuilder(tables, ignoreInvalidDefaults)

  override val columnTypes = new JdbcTypes
  override protected def computeQueryCompiler = super.computeQueryCompiler.replace(Phase.resolveZipJoinsRownumStyle) - Phase.fixRowNumberOrdering
  override def createQueryBuilder(n: Node, state: CompilerState): QueryBuilder = new QueryBuilder(n, state)
  override def createUpsertBuilder(node: Insert): InsertBuilder = new UpsertBuilder(node)
  override def createColumnDDLBuilder(column: FieldSymbol, table: Table[_]): ColumnDDLBuilder = new ColumnDDLBuilder(column)
  override def createInsertActionExtensionMethods[T](compiled: CompiledInsert): InsertActionExtensionMethods[T] =
    new CountingInsertActionComposerImpl[T](compiled)

  override def defaultSqlTypeName(tmd: JdbcType[_], sym: Option[FieldSymbol]): String = tmd.sqlType match {
    case java.sql.Types.VARCHAR =>
      val size = sym.flatMap(_.findColumnOption[RelationalProfile.ColumnOption.Length])
      size.fold("VARCHAR")(l => if(l.varying) s"VARCHAR(${l.length})" else s"CHAR(${l.length})")
    case _ => super.defaultSqlTypeName(tmd, sym)
  }

  class QueryBuilder(tree: Node, state: CompilerState) extends super.QueryBuilder(tree, state) {
    override protected val concatOperator = Some("||")
    override protected val alwaysAliasSubqueries = false
    override protected val supportsLiteralGroupBy = true
    override protected val quotedJdbcFns = Some(Nil)

    override def expr(n: Node, skipParens: Boolean = false) = n match {
      case Library.NextValue(SequenceNode(name))    => b"nextval(schema(), '$name')"
      case Library.CurrentValue(SequenceNode(name)) => b"currval(schema(), '$name')"
      case RowNumber(_) => b"rownum"
      case _ => super.expr(n, skipParens)
    }

    override protected def buildFetchOffsetClause(fetch: Option[Node], offset: Option[Node]) = (fetch, offset) match {
      case (Some(t), Some(d)) => b"\nlimit $t offset $d"
      case (Some(t), None   ) => b"\nlimit $t"
      case (None, Some(d)   ) => b"\nlimit -1 offset $d"
      case _ =>
    }
  }

  class ColumnDDLBuilder(column: FieldSymbol) extends super.ColumnDDLBuilder(column) {
    override protected def appendOptions(sb: StringBuilder): Unit = {
      if(defaultLiteral ne null) sb append " DEFAULT " append defaultLiteral
      if(notNull) sb append " NOT NULL"
      if(primaryKey) sb append " PRIMARY KEY"
      if(autoIncrement) sb append " AUTO_INCREMENT"
      if(unique) sb append " UNIQUE"
    }
  }

  class JdbcTypes extends super.JdbcTypes {
    override val uuidJdbcType = new UUIDJdbcType {
      override def sqlTypeName(sym: Option[FieldSymbol]) = "UUID"
      override def valueToSQLLiteral(value: UUID) = "'" + value + "'"
      override def hasLiteralForm = true
    }
    override val instantType : InstantJdbcType = new InstantJdbcType {
      // H2 doesn't use ISO-8601 format strings and so can't use Instant.parse and needs its own formatter
      val formatter = new DateTimeFormatterBuilder()
                      .append(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss"))
                      .optionalStart()
                      .appendFraction(ChronoField.NANO_OF_SECOND, 0, 9, true)
                      .optionalEnd()
                      .appendOffset("+HH", "")
                      .toFormatter()
      override def sqlTypeName(sym: Option[FieldSymbol]) = "TIMESTAMP(9) WITH TIME ZONE"

      override def setValue(v: Instant, p: PreparedStatement, idx: Int) : Unit = {
        p.setString(idx, if (v == null) null else v.toString)
      }
      override def getValue(r: ResultSet, idx: Int) : Instant = {
        r.getString(idx) match {
          case null => null
          case utcString => LocalDateTime.parse(utcString, formatter).toInstant(ZoneOffset.UTC)
        }
      }
      override def updateValue(v: Instant, r: ResultSet, idx: Int) = {
        r.updateString(idx, if (v == null) null else v.toString)
      }
      override def valueToSQLLiteral(value: Instant) : String = {
        s"'${value.toString}'"
      }
    }
  }

  /* Extending super.InsertBuilder here instead of super.UpsertBuilder. MERGE is almost identical to INSERT on H2. */
  class UpsertBuilder(ins: Insert) extends super.InsertBuilder(ins) {
    override protected def buildInsertStart = allNames.mkString(s"merge into $tableName (", ",", ") ")
  }

  class CountingInsertActionComposerImpl[U](compiled: CompiledInsert) extends super.CountingInsertActionComposerImpl[U](compiled) {
    // H2 cannot perform server-side insert-or-update with soft insert semantics. We don't have to do
    // the same in ReturningInsertInvoker because H2 does not allow returning non-AutoInc keys anyway.
    override protected val useServerSideUpsert = compiled.upsert.fields.forall(fs => !fs.options.contains(ColumnOption.AutoInc))
    override protected def useTransactionForUpsert = !useServerSideUpsert
  }
}

object H2Profile extends H2Profile
