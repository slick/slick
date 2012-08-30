package scala.slick.driver

import scala.slick.lifted._
import scala.slick.ast._
import scala.slick.ast.Util._
import scala.slick.ast.ExtraUtil._
import scala.slick.SlickException

/**
 * Slick driver for H2.
 *
 * This driver implements the [[scala.slick.driver.ExtendedProfile]] ''without'' the following
 * capabilities (see <a href="../../../index.html#scala.slick.driver.BasicProfile$$capabilities$" target="_parent">BasicProfile.capabilities</a>):
 *
 * <ul>
 *   <li><b>sequenceMin</b>, <b>sequenceMax</b>, <b>sequenceCycle</b>:
 *     H2 does not support MINVALUE, MAXVALUE and CYCLE</li>
 *   <li><b>returnInsertOther</b>: When returning columns from an INSERT
 *     operation, only a single column may be specified which must be the
 *     table's AutoInc column.</li>
 * </ul>
 *
 * @author szeiger
 */
trait H2Driver extends ExtendedDriver { driver =>

  override val capabilities: Set[Capability] = (BasicProfile.capabilities.all
    - BasicProfile.capabilities.sequenceMin
    - BasicProfile.capabilities.sequenceMax
    - BasicProfile.capabilities.sequenceCycle
    - BasicProfile.capabilities.returnInsertOther
  )

  override def createQueryBuilder(input: QueryBuilderInput): QueryBuilder = new QueryBuilder(input)

  override def mapTypeName(tmd: TypeMapperDelegate[_]): String = tmd.sqlType match {
    case java.sql.Types.VARCHAR => "VARCHAR"
    case _ => super.mapTypeName(tmd)
  }

  class QueryBuilder(input: QueryBuilderInput) extends super.QueryBuilder(input) {
    override protected val concatOperator = Some("||")

    override protected def toComprehension(n: Node, liftExpression: Boolean = false) =
      super.toComprehension(n, liftExpression) match {
        case c @ Comprehension(from, _, None, orderBy, Some(sel), _, _) if !orderBy.isEmpty && hasRowNumber(sel) =>
          // H2 supports only Oracle-style ROWNUM (applied before ORDER BY and GROUP BY),
          // so we pull the SELECT clause with the ROWNUM up into a new query
          val paths = findPaths(from.map(_._1).toSet, sel).map(p => (p, new AnonSymbol)).toMap
          val inner = c.copy(select = Some(Pure(StructNode(paths.toIndexedSeq.map { case (n,s) => (s,n) }))))
          val gen = new AnonSymbol
          val newSel = sel.replace {
            case s: Select => paths.get(s).fold(s) { sym => Select(Ref(gen), sym) }
          }
          Comprehension(Seq((gen, inner)), select = Some(newSel))
        case c => c
      }

    override def expr(n: Node, skipParens: Boolean = false) = n match {
      case Library.NextValue(SequenceNode(name)) => b += "nextval(schema(), '" += name += "')"
      case Library.CurrentValue(SequenceNode(name)) => b += "currval(schema(), '" += name += "')"
      case RowNumber(_) => b += "rownum()"
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
