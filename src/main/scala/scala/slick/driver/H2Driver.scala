package scala.slick.driver

import scala.slick.lifted._
import scala.slick.ast._
import scala.slick.jdbc.JdbcType
import scala.slick.util.MacroSupport.macroSupportInterpolation
import scala.slick.profile.{SqlProfile, Capability}
import scala.slick.compiler.CompilerState

/**
 * Slick driver for H2.
 *
 * This driver implements the [[scala.slick.driver.ExtendedProfile]]
 * ''without'' the following capabilities:
 *
 * <ul>
 *   <li>[[scala.slick.profile.SqlProfile.capabilities.sequenceMin]],
 *     [[scala.slick.profile.SqlProfile.capabilities.sequenceMax]],
 *     [[scala.slick.profile.SqlProfile.capabilities.sequenceCycle]]:
 *     H2 does not support MINVALUE, MAXVALUE and CYCLE</li>
 *   <li>[[scala.slick.driver.JdbcProfile.capabilities.returnInsertOther]]:
 *     When returning columns from an INSERT operation, only a single column
 *     may be specified which must be the table's AutoInc column.</li>
 * </ul>
 *
 * @author szeiger
 */
trait H2Driver extends JdbcDriver { driver =>

  override protected def computeCapabilities: Set[Capability] = (super.computeCapabilities
    - SqlProfile.capabilities.sequenceMin
    - SqlProfile.capabilities.sequenceMax
    - SqlProfile.capabilities.sequenceCycle
    - JdbcProfile.capabilities.returnInsertOther
  )

  override def createQueryBuilder(n: Node, state: CompilerState): QueryBuilder = new QueryBuilder(n, state)

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

    override protected def buildFetchOffsetClause(fetch: Option[Long], offset: Option[Long]) = (fetch, offset) match {
      case (Some(t), Some(d)) => b" limit $t offset $d"
      case (Some(t), None   ) => b" limit $t"
      case (None, Some(d)   ) => b" limit -1 offset $d"
      case _ =>
    }
  }
}

object H2Driver extends H2Driver
