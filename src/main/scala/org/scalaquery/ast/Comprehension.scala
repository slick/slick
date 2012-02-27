package org.scalaquery.ast

import OptimizerUtil._
import scala.collection.mutable.{ArrayBuffer, HashMap}
import org.scalaquery.ql.{AbstractTable, Join}
import org.scalaquery.util.Logging

case class Comprehension(from: Seq[(Symbol, Node)], where: Seq[Node], select: Option[Node]) extends Node with DefNode {
  protected[this] def nodeChildGenerators = from.map(_._2) ++ where ++ select
  override protected[this] def nodeChildNames = from.map("from " + _._1) ++ where.zipWithIndex.map("where" + _._2) ++ select.map(_ => "select")
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

object Comprehension extends Logging {
  def toComprehensions(tree: Node) = {
    val t2 = convert(tree)
    logger.debug("converted: ", t2)
    inline(t2)
  }

  val convert = new Transformer {
    def replace = {
      // Bind to Comprehension
      case Bind(gen, from, select) => Comprehension(Seq((gen, from)), Nil, Some(select))
      // Filter to Comprehension
      case Filter(gen, from, where) => Comprehension(Seq((gen, from)), Seq(where), None)
      // Merge Comprehension which selects another Comprehension
      case Comprehension(from1, where1, Some(c2 @ Comprehension(from2, where2, select))) =>
        c2.copy(from = from1 ++ from2, where = where1 ++ where2)
      // Turn explicit inner join into an implicit one
      case f @ FilteredJoin(leftGen, rightGen, left, right, Join.Inner, on) =>
        Comprehension(Seq((leftGen, left), (rightGen, right)), Seq(on), None)
    }
  }

  def inline(tree: Node) = {
    object ComprehensionStruct {
      def unapply(c: Comprehension): Option[Node] = c match {
        case Comprehension(_, _, Some(Pure(s : StructNode))) => Some(s)
        case Comprehension(_, _, Some(Pure(t: TableRef))) => Some(t)
        case Comprehension(_, _, Some(c2: Comprehension)) => unapply(c2)
        case Comprehension(from, _, None) =>
          from.last._2 match {
            case c2: Comprehension => unapply(c2)
            case p @ Pure(t: TableRef) => Some(TableRef(from.last._1))
            case _ => None
          }
        case _ => None
      }
    }
    def protectedRefs = tree.collect[Symbol]{ case TableRef(sym) => sym }.toSet
    logger.debug("protected refs: "+protectedRefs)

    def f(n: Node): Node = n match {
      case c: Comprehension =>
        val newGens = new ArrayBuffer[(Symbol, Node)]
        val newWhere = new ArrayBuffer[Node]
        val eliminated = new HashMap[Symbol, Node]
        var rewrite = false
        def scanFrom(c: Comprehension): Unit = {
          c.from.foreach {
            case t @ (s,  n @ ComprehensionStruct(target)) if(!protectedRefs(s)) =>
              rewrite = true
              eliminated += ((s, target))
              scanFrom(n)
            case t =>
              newGens += t
          }
          newWhere ++= c.where
        }
        scanFrom(c)
        logger.debug("eliminated: "+eliminated)
        def replaceRefs(n: Node): Node = n match {
          case Path(t, c) if eliminated.contains(t) =>
            eliminated(t) match {
              case TableRef(tab) =>
                logger.debug("replacing Path("+t+", "+c+") by Path("+tab+", "+c+") [TableRef]")
                replaceRefs(Path(tab, c))
              case StructNode(mapping) =>
                logger.debug("replacing Path("+t+", "+c+") by "+mapping.toMap.apply(c))
                replaceRefs(mapping.toMap.apply(c))
              case _ => n
            }
          case n => n.nodeMapChildren(replaceRefs)
        }
        if(rewrite) replaceRefs(Comprehension(newGens, newWhere, c.select)).nodeMapChildren(f)
        else c.nodeMapChildren(f)
      case n => n.nodeMapChildren(f)
    }
    f(tree)
  }

  def filterSource(n: Node): Node = n match {
    case Comprehension(from, _, None) => filterSource(from.last._2)
    case n => n
  }
}
