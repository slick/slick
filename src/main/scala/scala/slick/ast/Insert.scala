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
  def nodeWithComputedType(scope: SymbolScope, retype: Boolean): Self = if(nodeHasType && !retype) this else {
    val t2 = table.nodeWithComputedType(scope, retype)
    val m2 = map.nodeWithComputedType(scope + (generator -> t2.nodeType), retype)
    val newType = m2.nodeType
    if((t2 eq table) && (m2 eq map) && newType == nodeType) this
    else copy(table = t2, map = m2).nodeTyped(newType)
  }
}
