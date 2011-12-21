package org.scalaquery.ast

import OptimizerUtil._
import org.scalaquery.ql.{TableQuery, Filter, Pure, Bind}


/**
 * Expand columns in queries
 */
object Columnizer {

  val bindToComprehensions = refMemoized[Node, Node](r => {
    case Bind(bfrom, bselect) => r(bselect) match {
      case Comprehension(from, where, select) => r(Comprehension(r(bfrom) +: from, where, select))
      case n => r(Comprehension(Seq(r(bfrom)), Seq.empty, Seq(n)))
    }
    case Filter(from, where) => r(Comprehension(Seq(r(from)), Seq(r(where)), Seq.empty))
    //case Bind(from, Pure(value)) => r(Comprehension(Seq(r(from)), Seq.empty, Seq(r(value))))
    /*case Bind(from1, Comprehension(from2, where, select)) =>
      r(Comprehension(r(from1) +: from2.map(r), where.map(r), select.map(r)))
    case TableQuery(table) => r(Comprehension(Seq(r(table)), Seq.empty, Seq.empty))
    case Comprehension(from, where, select) if hasSimpleComprehension(from) =>
      r(Comprehension(unwrapSimpleComprehensions(from), where, select))
    case Comprehension(Seq(Pure(value)), where, Seq()) => r(Comprehension(Seq.empty, where, Seq(value)))*/
    case n =>
      val nn = n.nodeMapChildren(r)
      if(nn eq n) n else r(nn)
  })

  def hasSimpleComprehension(s: Seq[Node]) = s.exists(n => n match {
    case Comprehension(Seq(from), Seq(), Seq()) => true
    case _ => false
  })

  def unwrapSimpleComprehensions(s: Seq[Node]) = s.map {
    case Comprehension(Seq(from), Seq(), Seq()) => from
    case n => n
  }

  val mergeComprehensions = memoized[Node, Node](r => {
    case n => n
  })
  /*private def mergeFrom(c: Comprehension): Comprehension =  {

  }*/

  def recoverSymbols = {

  }
}

case class Comprehension(from: Seq[Node], where: Seq[Node], select: Seq[Node]) extends Node {
  protected[this] def nodeChildGenerators = from ++ where ++ select
  override protected[this] def nodeChildNames = from.zipWithIndex.map("from" + _._2) ++ where.zipWithIndex.map("where" + _._2) ++ select.zipWithIndex.map("select" + _._2)
  def nodeMapChildren(f: Node => Node) = {
    val fromO = nodeMapNodes(from, f)
    val whereO = nodeMapNodes(where, f)
    val selectO = nodeMapNodes(select, f)
    if(fromO.isDefined || whereO.isDefined || selectO.isDefined)
      copy(from = fromO.getOrElse(from), where = whereO.getOrElse(where), select = selectO.getOrElse(select))
    else this
  }
  override def toString = "Comprehension"
}

case class Path(child: Node, name: Seq[String]) extends UnaryNode {
  protected[this] def nodeRebuild(child: Node) = copy(child = child)
}
