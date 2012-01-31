package org.scalaquery.ast

import scala.collection.mutable.{HashMap, ArrayBuffer}
import org.scalaquery.util.RefId
import org.scalaquery.ql.{RawNamedColumn, AbstractTable}
import OptimizerUtil._
import Optimizer.{FilterChain, InRefChain, NestedProductNode}

/**
 * Expand columns and merge comprehensions in queries
 */
object Columnizer {

  val expandColumns = new Transformer.Defs {
    /*override def initTree(tree: Node) {
      super.initTree(tree)
      tree.collectAll[(AbstractTable[_], RawNamedColumn)] {
        case ResolvedInRef(_, in, r @ RawNamedColumn(name, _, _)) => expand(in).map((_, r)).toSeq
      } foreach { case (t,c) =>
        println("*** "+t.tableName+"."+c.name)
      }
      def expand(n: Node): Iterator[AbstractTable[_]] = n match {
        case a: AbstractTable[_] => Iterator(a)
        case ResolvedRef(_, target) => expand(target)
        case InRef(_, what) => expand(what)
        case FilteredQuery(_, from) => expand(from)
        case Pure(value) => expand(value)
        case Bind(_, _, select) => expand(select)
        case Union(left, right, _, _, _) => expand(left) ++ expand(right)
        case _ => Iterator.empty
      }
    }*/
    def replace = pftransitive {
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

  def unwrap(wrappers: Set[Symbol], n: Node): Node = n.replace {
    case InRef(sym, what) if wrappers contains sym => what
  }

  val mergePaths = new Transformer.Defs {
    def replace = {
      /*case InRef(sym, RawNamedColumn(name, _, _)) => Path(sym, FieldSymbol(name))
      case InRef(sym, Path(syms @ _*)) => Path((sym +: syms): _*)
      case Ref(sym) => Path(sym)*/
      case Ref(null) => null
    }
  }

  def run(tree: Node): Node = {
    //val t3 = toComprehensions.andThen(mergeComprehensions).apply(t2)
    //val cs = findColumns(withCs)
    //println("*** cs: "+cs)
    //findDefs(withCs)
    //introduceRefs(withCs)
    tree
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

  def findDefs(n: Node) = n.collectAll[(Symbol, (Node, Node))] {
    case n: DefNode => n.nodeGenerators.map { case (sym, what) => (sym, (what, n)) }
  }.toMap
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
}
