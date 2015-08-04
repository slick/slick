package slick.driver

import java.util.UUID

import scala.concurrent.ExecutionContext
import slick.ast._
import slick.util.MacroSupport.macroSupportInterpolation
import slick.profile.{RelationalProfile, SqlProfile, Capability}
import slick.compiler.{Phase, CompilerState}
import slick.jdbc.{JdbcModelBuilder, JdbcType}
import slick.jdbc.meta.{MColumn, MTable}
import slick.model.Model

/** Slick driver for H2.
  *
  * This driver implements [[slick.driver.JdbcProfile]]
  * ''without'' the following capabilities:
  *
  * <ul>
  *   <li>[[slick.profile.RelationalProfile.capabilities.reverse]]:
  *     This String function is not available in H2.</li>
  *   <li>[[slick.profile.SqlProfile.capabilities.sequenceMin]],
  *     [[slick.profile.SqlProfile.capabilities.sequenceMax]],
  *     [[slick.profile.SqlProfile.capabilities.sequenceCycle]]:
  *     H2 does not support MINVALUE, MAXVALUE and CYCLE</li>
  *   <li>[[slick.driver.JdbcProfile.capabilities.returnInsertOther]]:
  *     When returning columns from an INSERT operation, only a single column
  *     may be specified which must be the table's AutoInc column.</li>
  *   <li>[[slick.profile.RelationalProfile.capabilities.joinFull]]:
  *     Full outer joins are emulated because there is not native support
  *     for them.</li>
  *   <li>[[slick.driver.JdbcProfile.capabilities.insertOrUpdate]]:
  *     InsertOrUpdate operations are emulated on the client side if the
  *     data to insert contains an `AutoInc` fields. Otherwise the operation
  *     is performmed natively on the server side.</li>
  * </ul>
  */
trait H2Driver extends JdbcDriver { driver =>

  override protected def computeCapabilities: Set[Capability] = (super.computeCapabilities
    - SqlProfile.capabilities.sequenceMin
    - SqlProfile.capabilities.sequenceMax
    - SqlProfile.capabilities.sequenceCycle
    - JdbcProfile.capabilities.returnInsertOther
    - RelationalProfile.capabilities.joinFull
    - JdbcProfile.capabilities.insertOrUpdate
    - RelationalProfile.capabilities.reverse
  )

  class ModelBuilder(mTables: Seq[MTable], ignoreInvalidDefaults: Boolean)(implicit ec: ExecutionContext) extends JdbcModelBuilder(mTables, ignoreInvalidDefaults) {
    override def createTableNamer(mTable: MTable): TableNamer = new TableNamer(mTable) {
      override def schema = super.schema.filter(_ != "PUBLIC") // remove default schema
    }
    override def createColumnBuilder(tableBuilder: TableBuilder, meta: MColumn): ColumnBuilder = new ColumnBuilder(tableBuilder, meta) {
      override def length = super.length.filter(_ != Int.MaxValue) // H2 sometimes show this value, but doesn't accept it back in the DBType
      override def default = rawDefault.map((_,tpe)).collect{
          case (v,"java.util.UUID") => Some(Some(java.util.UUID.fromString(v.replaceAll("[\'\"]", "")))) //strip quotes
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
  override def createInsertActionExtensionMethods[T](compiled: CompiledInsert): InsertActionExtensionMethods[T] =
    new CountingInsertActionComposerImpl[T](compiled)

  override def defaultSqlTypeName(tmd: JdbcType[_], size: Option[RelationalProfile.ColumnOption.Length]): String = tmd.sqlType match {
    case java.sql.Types.VARCHAR =>
      size.fold("VARCHAR")(l => if(l.varying) s"VARCHAR(${l.length})" else s"CHAR(${l.length})")
    case _ => super.defaultSqlTypeName(tmd, size)
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

  class JdbcTypes extends super.JdbcTypes {
    override val uuidJdbcType = new UUIDJdbcType {
      override def sqlTypeName(size: Option[RelationalProfile.ColumnOption.Length]) = "UUID"
      override def valueToSQLLiteral(value: UUID) = "'" + value + "'"
      override def hasLiteralForm = true
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

object H2Driver extends H2Driver
