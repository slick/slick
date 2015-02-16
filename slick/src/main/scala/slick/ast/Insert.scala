package slick.ast

/** Represents an Insert operation. */
final case class Insert(tableSym: Symbol, table: Node, linear: Node) extends BinaryNode with DefNode {
  type Self = Insert
  def left = table
  def right = linear
  override def nodeChildNames = Vector("table "+tableSym, "linear")
  def nodeGenerators = Vector((tableSym, table))
  def nodeRebuild(l: Node, r: Node) = copy(table = l, linear = r)
  def nodeRebuildWithGenerators(gen: IndexedSeq[Symbol]) = copy(tableSym = gen(0))
  def nodeWithComputedType2(scope: SymbolScope, typeChildren: Boolean, retype: Boolean): Self = {
    val table2 = table.nodeWithComputedType(scope, typeChildren, retype)
    val lin2 = linear.nodeWithComputedType(scope + (tableSym -> table2.nodeType), typeChildren, retype)
    nodeRebuildOrThis(Vector(table2, lin2)).nodeTypedOrCopy(if(!nodeHasType || retype) lin2.nodeType else nodeType)
  }
  override def getDumpInfo = super.getDumpInfo.copy(mainInfo = "")
}

/** A column in an Insert operation. */
final case class InsertColumn(children: IndexedSeq[Node], fs: FieldSymbol, tpe: Type) extends Node with TypedNode {
  def nodeChildren = children
  type Self = InsertColumn
  protected[this] def nodeRebuild(ch: IndexedSeq[Node]) = copy(children = ch)
  override def getDumpInfo = super.getDumpInfo.copy(mainInfo = fs.toString)
}
