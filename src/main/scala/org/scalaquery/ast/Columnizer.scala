package org.scalaquery.ast

import OptimizerUtil._
import org.scalaquery.ql.{TableQuery, Filter, Pure, Bind}
import collection.mutable.HashMap
import org.scalaquery.util.RefId

/**
 * Expand columns and merge comprehensions in queries
 */
class Columnizer {

  def run(tree: Node): Node = {
    val withCs = toComprehensions.andThen(mergeComprehensions).apply(tree)
    val cs = findColumns(withCs)
    println("*** cs: "+cs)
    //findDefs(withCs)
    //introduceRefs(withCs)
    withCs
  }

  val toComprehensions = new Transformer {
    def replace = {
      case Bind(gen, from, select) => Comprehension(Seq((gen, from)), Nil, Some(select))
      case Filter(gen, from, where) => Comprehension(Seq((gen, from)), Seq(where), None)
    }
  }

  val mergeComprehensions = new Transformer {
    def replace = {
      case c1 @ Comprehension(from1, where1, Some(c2 @ Comprehension(from2, where2, select))) =>
        c2.copy(from = from1 ++ from2, where = where1 ++ where2)
    }
  }

  def findColumns(n: Node) = n.collect[(Symbol, Option[Node])] {
    case Ref(sym) => (sym, None)
    case InRef(sym, value) => (sym, Some(value))
  }.toSet
}

case class Comprehension(from: Seq[(Symbol, Node)], where: Seq[Node], select: Option[Node]) extends Node with DefNode {
  protected[this] def nodeChildGenerators = from.map(_._2) ++ where ++ select
  override protected[this] def nodeChildNames = from.map("from " + _._1) ++ where.zipWithIndex.map("where" + _._2) ++ select.zipWithIndex.map("select" + _._2)
  def nodeMapChildren(f: Node => Node) = mapChildren(f, f)
  def mapChildren(fromMap: Node => Node, otherMap: Node => Node): Node = {
    val fromO = nodeMapNodes(from.view.map(_._2), fromMap)
    val whereO = nodeMapNodes(where, otherMap)
    val selectO = select.map(otherMap)
    if(fromO.isDefined || whereO.isDefined || selectO != select)
      copy(from = fromO.map(f => from.view.map(_._1).zip(f)).getOrElse(from), where = whereO.getOrElse(where), select = selectO)
    else this
  }
  def nodeGenerators = from
  override def toString = "Comprehension"
}
