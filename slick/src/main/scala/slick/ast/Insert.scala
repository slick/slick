package slick.ast

/** Represents an Insert operation. */
final case class Insert(tableSym: TermSymbol, table: Node, linear: Node) extends BinaryNode with DefNode {
  type Self = Insert
  def left = table
  def right = linear
  override def childNames = Vector("table "+tableSym, "linear")
  def generators = Vector((tableSym, table))
  def rebuild(l: Node, r: Node) = copy(table = l, linear = r)
  def rebuildWithSymbols(gen: IndexedSeq[TermSymbol]) = copy(tableSym = gen(0))
  def withInferredType(scope: Type.Scope, typeChildren: Boolean): Self = {
    val table2 = table.infer(scope, typeChildren)
    val lin2 = linear.infer(scope + (tableSym -> table2.nodeType), typeChildren)
    withChildren(Vector(table2, lin2)) :@ (if(!hasType) lin2.nodeType else nodeType)
  }
  override def getDumpInfo = super.getDumpInfo.copy(mainInfo = "")
}

/** A column in an Insert operation. */
final case class InsertColumn(children: IndexedSeq[Node], fs: FieldSymbol, buildType: Type) extends Node with SimplyTypedNode {
  type Self = InsertColumn
  protected[this] def rebuild(ch: IndexedSeq[Node]) = copy(children = ch)
  override def getDumpInfo = super.getDumpInfo.copy(mainInfo = fs.toString)
}
