package org.scalaquery.ast

import scala.collection.mutable.{HashMap, ArrayBuffer}
import org.scalaquery.util.RefId
import org.scalaquery.ql.{RawNamedColumn, AbstractTable}
import OptimizerUtil._

/**
 * Expand columns and merge comprehensions in queries
 */
object Columnizer extends (Node => Node) {

  val expandColumns = new Transformer.Defs {
    def replace = pftransitive {
      // Remove unnecessary wrapping of generated TableRef reference
      case ResolvedInRef(sym, Pure(TableRef(sym1)), InRef(sym2, what)) if sym1 == sym2 => InRef(sym, what)
      // Rewrite a table reference that has already been rewritten to a Ref
      case ResolvedRef(sym, f @ FilterChain(syms, t: AbstractTable[_])) => InRef(sym, Node(t.*))
      // Push InRef down into ProductNode
      case InRef(sym, ProductNode(ns @ _*)) => ProductNode(ns.map(n => InRef(sym, n)): _*)
      // Merge products
      case NestedProductNode(ch @ _*) => ProductNode(ch: _*)
      // Rewrite a table reference returned in a Bind
      case b @ Bind(_, _, t: AbstractTable[_]) => b.copy(select = Bind(new AnonSymbol, t, Pure(Node(t.*))))
      case Pure(ResolvedRef(sym1, f @ FilterChain(syms, t: AbstractTable[_]))) => Pure(TableRef(sym1))
    }
  }

  def apply(tree: Node): Node = {
    val n = expandAndOptimize(tree)
    if(n == tree) n else apply(n)
  }

  def expandAndOptimize(tree: Node): Node = {
    val t2 = expandColumns(tree)
    //TODO This hack unwraps the expanded references within their scope
    // There may be other situations where unwrapping finds the wrong symbols,
    // so this should be done in a completely different way (by introducing
    // StructNodes much earlier and avoiding wrapping altogether)
    def optimizeUnions(n: Node): Node = n match {
      case u @ Union(left, right, _, _, _) =>
        val l = Optimizer.all(optimizeUnions(left))
        val r = Optimizer.all(optimizeUnions(right))
        u.copy(left = l, right = r)
      case n => n.nodeMapChildren(optimizeUnions)
    }
    val t3 = optimizeUnions(t2)
    Optimizer.all(t3)
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
  def nodeMapGenerators(f: Symbol => Symbol) = {
    val gens = from.map(_._1)
    mapOrNone(gens, f) match {
      case Some(s) => copy(from = from.zip(s).map { case ((_, n), s) => (s, n) })
      case None => this
    }
  }
  def nodePostGeneratorChildren = select.toSeq
  def nodeMapScopedChildren(f: (Option[Symbol], Node) => Node) = {
    val fn = (n: Node) => f(None, n)
    val from2 = from.map{ case (s, n) => f(Some(s), n) }
    val fromO = if(from.zip(from2).forall{ case ((_, n1), n2) => n1 eq n2 }) None else Some(from2)
    val whereO = nodeMapNodes(where, fn)
    val selectO = select.map(fn)
    if(fromO.isDefined || whereO.isDefined || selectO != select)
      copy(from = fromO.map(f => from.view.map(_._1).zip(f)).getOrElse(from), where = whereO.getOrElse(where), select = selectO)
    else this
  }
}
