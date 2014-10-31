package scala.slick.driver

import scala.slick.ast._
import scala.slick.util.MacroSupport.macroSupportInterpolation
import scala.slick.profile.{RelationalProfile, SqlProfile, Capability}
import scala.slick.compiler.CompilerState
import scala.slick.jdbc.JdbcType
import scala.slick.jdbc.meta.MTable
import scala.slick.model.Model

/** Slick driver for H2.
  *
  * This driver implements [[scala.slick.driver.JdbcProfile]]
  * ''without'' the following capabilities:
  *
  * <ul>
  *   <li>[[scala.slick.profile.RelationalProfile.capabilities.reverse]]:
  *     This String function is not available in H2.</li>
  *   <li>[[scala.slick.profile.SqlProfile.capabilities.sequenceMin]],
  *     [[scala.slick.profile.SqlProfile.capabilities.sequenceMax]],
  *     [[scala.slick.profile.SqlProfile.capabilities.sequenceCycle]]:
  *     H2 does not support MINVALUE, MAXVALUE and CYCLE</li>
  *   <li>[[scala.slick.driver.JdbcProfile.capabilities.returnInsertOther]]:
  *     When returning columns from an INSERT operation, only a single column
  *     may be specified which must be the table's AutoInc column.</li>
  *   <li>[[scala.slick.profile.RelationalProfile.capabilities.joinFull]]:
  *     Full outer joins are emulated because there is not native support
  *     for them.</li>
  *   <li>[[scala.slick.driver.JdbcProfile.capabilities.insertOrUpdate]]:
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

  class ModelBuilder(mTables: Seq[MTable], ignoreInvalidDefaults: Boolean = true)(implicit session: Backend#Session) extends super.ModelBuilder(mTables, ignoreInvalidDefaults){
    override def Table = new Table(_){
      override def schema = super.schema.filter(_ != "PUBLIC") // remove default schema
      override def Column = new Column(_){
        override def length = super.length.filter(_ != Int.MaxValue) // H2 sometimes show this value, but doesn't accept it back in the DBType
        override def default = meta.columnDef.map((_,tpe)).collect{
            case (v,"java.util.UUID")  => Some(Some(java.util.UUID.fromString(v.replaceAll("[\'\"]", "")))) //strip quotes
          }.getOrElse{super.default}
        override def tpe = dbType match {
          case Some("UUID") => "java.util.UUID"
          case _ => super.tpe
        }
      }
    }
  }
   
  override def createModel(tables: Option[Seq[MTable]] = None, ignoreInvalidDefaults: Boolean = true)
                          (implicit session: Backend#Session)
                          : Model
    = new ModelBuilder(tables.getOrElse(defaultTables), ignoreInvalidDefaults).model

  override val columnTypes = new JdbcTypes
  override def createQueryBuilder(n: Node, state: CompilerState): QueryBuilder = new QueryBuilder(n, state)
  override def createUpsertBuilder(node: Insert): InsertBuilder = new UpsertBuilder(node)
  override def createCountingInsertInvoker[U](compiled: CompiledInsert) = new CountingInsertInvoker[U](compiled)

  override def defaultSqlTypeName(tmd: JdbcType[_]): String = tmd.sqlType match {
    case java.sql.Types.VARCHAR => "VARCHAR"
    case _ => super.defaultSqlTypeName(tmd)
  }

  class QueryBuilder(tree: Node, state: CompilerState) extends super.QueryBuilder(tree, state)  with OracleStyleRowNum {
    override protected val concatOperator = Some("||")

    override def expr(n: Node, skipParens: Boolean = false) = n match {
      case Library.NextValue(SequenceNode(name))    => b"nextval(schema(), '$name')"
      case Library.CurrentValue(SequenceNode(name)) => b"currval(schema(), '$name')"
      case _ => super.expr(n, skipParens)
    }

    override protected def buildFetchOffsetClause(fetch: Option[Node], offset: Option[Node]) = (fetch, offset) match {
      case (Some(t), Some(d)) => b" limit $t offset $d"
      case (Some(t), None   ) => b" limit $t"
      case (None, Some(d)   ) => b" limit -1 offset $d"
      case _ =>
    }
  }

  class JdbcTypes extends super.JdbcTypes {
    override val uuidJdbcType = new UUIDJdbcType {
      override def sqlTypeName = "UUID"
    }
  }

  /* Extending super.InsertBuilder here instead of super.UpsertBuilder. MERGE is almost identical to INSERT on H2. */
  class UpsertBuilder(ins: Insert) extends super.InsertBuilder(ins) {
    override protected def buildInsertStart = allNames.mkString(s"merge into $tableName (", ",", ") ")
  }

  class CountingInsertInvoker[U](compiled: CompiledInsert) extends super.CountingInsertInvoker[U](compiled) {
    // H2 cannot perform server-side insert-or-update with soft insert semantics. We don't have to do
    // the same in ReturningInsertInvoker because H2 does not allow returning non-AutoInc keys anyway.
    override protected val useServerSideUpsert = compiled.upsert.fields.forall(fs => !fs.options.contains(ColumnOption.AutoInc))
    override protected def useTransactionForUpsert = !useServerSideUpsert
  }
}

object H2Driver extends H2Driver
