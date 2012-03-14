package org.scalaquery.ql

import org.scalaquery.ast.{UnaryNode, Node}


trait QueryModifier extends Node

final case class Grouping(child: Node) extends QueryModifier with UnaryNode {
  def by = child
  protected[this] def nodeRebuild(child: Node): Node = copy(child = child)
  protected[this] override def nodeChildNames = Seq("by")
  override def toString = "Grouping"
}
