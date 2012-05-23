package scala.slick.driver

import scala.slick.ql._
import scala.slick.ast._
import scala.slick.util.ValueLinearizer

/**
 * SLICK driver for H2.
 *
 * All ExtendedProfile features are supported.
 *
 * @author szeiger
 */
trait H2Driver extends ExtendedDriver { driver =>

  override def createQueryBuilder(node: Node, vl: ValueLinearizer[_]): QueryBuilder = new QueryBuilder(node, vl)

  override def mapTypeName(tmd: TypeMapperDelegate[_]): String = tmd.sqlType match {
    case java.sql.Types.VARCHAR => "VARCHAR"
    case _ => super.mapTypeName(tmd)
  }

  class QueryBuilder(ast: Node, linearizer: ValueLinearizer[_]) extends super.QueryBuilder(ast, linearizer) {
    override protected val concatOperator = Some("||")

    override def expr(n: Node, skipParens: Boolean = false) = n match {
      case Sequence.Nextval(seq) => b += "nextval(schema(), '" += seq.name += "')"
      case Sequence.Currval(seq) => b += "currval(schema(), '" += seq.name += "')"
      case _ => super.expr(n, skipParens)
    }

    override protected def buildFetchOffsetClause(fetch: Option[Long], offset: Option[Long]) = (fetch, offset) match {
      case (Some(t), Some(d)) => b += " LIMIT " += t += " OFFSET " += d
      case (Some(t), None) => b += " LIMIT " += t
      case (None, Some(d)) => b += " LIMIT -1 OFFSET " += d
      case _ =>
    }
  }
}

object H2Driver extends H2Driver
