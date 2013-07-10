package scala.slick.ast

/** Represents an Insert operation. */
final case class Insert(generator: Symbol, table: Node, map: Node, linear: Node) extends Node with DefNode {
  type Self = Insert
  def left = table
  def right = map
  override def nodeChildNames = Vector("table", "map", "linear")
  def nodeChildren = Vector(table, map, linear)
  def nodeGenerators = Vector((generator, table))
  def nodeRebuild(ch: IndexedSeq[Node]) = copy(table = ch(0), map = ch(1), linear = ch(2))
  def nodeRebuildWithGenerators(gen: IndexedSeq[Symbol]) = copy(generator = gen(0))
  def nodeWithComputedType2(scope: SymbolScope, typeChildren: Boolean, retype: Boolean): Self = {
    val table2 = table.nodeWithComputedType(scope, typeChildren, retype)
    val map2 = map.nodeWithComputedType(scope + (generator -> table2.nodeType), typeChildren, retype)
    nodeRebuildOrThis(Vector(table2, map2, linear)).nodeTypedOrCopy(if(!nodeHasType || retype) map2.nodeType else nodeType)
  }
}
