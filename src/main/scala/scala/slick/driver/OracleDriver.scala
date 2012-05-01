package scala.slick.driver

import scala.slick.ql._
import scala.slick.ast._
import scala.slick.util.ValueLinearizer

class OracleDriver extends ExtendedDriver { driver =>

  override def createQueryBuilder(node: Node, vl: ValueLinearizer[_]): QueryBuilder = new QueryBuilder(node, vl)

  class QueryBuilder(ast: Node, linearizer: ValueLinearizer[_]) extends super.QueryBuilder(ast, linearizer) {
    override protected val scalarFrom = Some("DUAL")
    override protected val concatOperator = Some("||")

    /*TODO
    override protected def innerBuildSelectNoRewrite(rename: Boolean) {
      query.typedModifiers[TakeDrop] match {
        case TakeDrop(Some(t), None) :: _ =>
          b += "SELECT * FROM (SELECT "
          expr(query.reified)
          //TODO fromSlot = b.createSlot
          appendClauses()
          b += ") WHERE ROWNUM <= " += t
        case TakeDrop(to, Some(d)) :: _ =>
          b += "SELECT * FROM (SELECT t0.*, ROWNUM ROWNUM_O FROM (SELECT "
          expr(Node(query.reified))
          b += ",ROWNUM ROWNUM_I"
          //TODO fromSlot = b.createSlot
          appendClauses()
          b += ") t0) WHERE ROWNUM_O"
          to match {
            case Some(t) =>
              b += " BETWEEN (1+" += d += ") AND (" += d += "+" += t += ")"
            case None =>
              b += ">" += d
          }
          b += " ORDER BY ROWNUM_I"
        case _ =>
          b += "SELECT "
          expr(query.reified)
          //TODO fromSlot = b.createSlot
          appendClauses()
      }
    }
    */

    override protected def appendTakeDropClause(take: Option[Int], drop: Option[Int]) = ()
  }
}

object OracleDriver extends OracleDriver
