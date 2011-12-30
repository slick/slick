package org.scalaquery.ast

import OptimizerUtil._
import org.scalaquery.ql.{TableQuery, Filter, Pure, Bind}
import collection.mutable.HashMap
import org.scalaquery.util.RefId


/**
 * Expand columns in queries
 */
class Columnizer {

  var nextNum = 1
  var defs = new HashMap[RefId[Node], (Symbol, Node)]

  def run(tree: Node): Node = {
    val withCs = bindToComprehensions(tree)
    findDefs(withCs)
    introduceRefs(withCs)
  }

  def makeSym = {
    val n = nextNum
    nextNum += 1
    new Symbol("s"+n)
  }

  def findDefs(n: Node) = n.foreach {
    case in @ Comprehension(from, _, _) =>
      from.foreach { case (sym, node) =>
        defs += ((RefId(node), (sym, in)))
      }
    case _ =>
  }

  def introduceRefs(n: Node) = memoized[(RefId[Node], Boolean), Node](r => { case (rn @ RefId(n), here) =>
    val n2 = if(here) {
      defs.get(rn).map { case (sym, in) =>
        Ref(n, Seq(sym))
      } getOrElse n
    } else n
    n2 match {
      case c @ Comprehension(_, _, _) => c.mapChildren(n => r((RefId(n), false)), n => r((RefId(n), true)))
      case n => n.nodeMapChildren(n => r((RefId(n), true)))
    }
  }).apply((RefId(n), true))

  val bindToComprehensions = refMemoized[Node, Node](r => {
    case Bind(bfrom, bselect) => r(bselect) match {
      case Comprehension(from, where, select) => r(Comprehension((makeSym, r(bfrom)) +: from, where, select))
      case n => r(Comprehension(Seq((makeSym, r(bfrom))), Seq.empty, Seq(n)))
    }
    case Filter(from, where) => r(Comprehension(Seq((makeSym, r(from))), Seq(r(where)), Seq.empty))
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
}

case class Comprehension(from: Seq[(Symbol, Node)], where: Seq[Node], select: Seq[Node]) extends Node {
  protected[this] def nodeChildGenerators = from.map(_._2) ++ where ++ select
  override protected[this] def nodeChildNames = from.map("from " + _._1) ++ where.zipWithIndex.map("where" + _._2) ++ select.zipWithIndex.map("select" + _._2)
  def nodeMapChildren(f: Node => Node) = mapChildren(f, f)
  def mapChildren(fromMap: Node => Node, otherMap: Node => Node): Node = {
    val fromO = nodeMapNodes(from.view.map(_._2), fromMap)
    val whereO = nodeMapNodes(where, otherMap)
    val selectO = nodeMapNodes(select, otherMap)
    if(fromO.isDefined || whereO.isDefined || selectO.isDefined)
      copy(from = fromO.map(f => from.view.map(_._1).zip(f)).getOrElse(from), where = whereO.getOrElse(where), select = selectO.getOrElse(select))
    else this
  }
  override def toString = "Comprehension"
}

case class Ref(child: Node, path: Seq[Symbol]) extends UnaryNode {
  protected[this] def nodeRebuild(child: Node) = copy(child = child)
  override def toString = "Ref "+path.mkString(".")
}

class Symbol(val name: String) {
  override def toString = name
}
