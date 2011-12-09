package org.scalaquery.ql

import org.scalaquery.util.{UnaryNode, Node}

trait QueryModifier extends Node

sealed abstract class Ordering extends QueryModifier with UnaryNode {
  def by = child
  val nullOrdering: Ordering.NullOrdering
  protected[this] override def nodeChildNames = Seq("by")
  def nullsFirst: Ordering
  def nullsLast: Ordering
}

object Ordering {
  final case class Asc(child: Node, nullOrdering: Ordering.NullOrdering = Ordering.NullsDefault) extends Ordering {
    override def toString = "Ordering.Asc"
    def nullsFirst = copy(nullOrdering = Ordering.NullsFirst)
    def nullsLast = copy(nullOrdering = Ordering.NullsLast)
    protected[this] def nodeRebuild(child: Node): Node = copy(child = child)
  }

  final case class Desc(child: Node, nullOrdering: Ordering.NullOrdering = Ordering.NullsDefault) extends Ordering {
    override def toString = "Ordering.Desc"
    def nullsFirst = copy(nullOrdering = Ordering.NullsFirst)
    def nullsLast = copy(nullOrdering = Ordering.NullsLast)
    protected[this] def nodeRebuild(child: Node): Node = copy(child = child)
  }

  sealed trait NullOrdering
  final case object NullsDefault extends NullOrdering
  final case object NullsFirst extends NullOrdering
  final case object NullsLast extends NullOrdering
}

final case class Grouping(child: Node) extends QueryModifier with UnaryNode {
  def by = child
  protected[this] def nodeRebuild(child: Node): Node = copy(child = child)
  protected[this] override def nodeChildNames = Seq("by")
  override def toString = "Grouping"
}
