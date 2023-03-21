package slick.ast

import slick.util.ConstArray


/** Represents an Insert operation. */
final case class Insert(tableSym: TermSymbol, table: Node, linear: Node, allFields: ConstArray[FieldSymbol])
  extends BinaryNode
    with DefNode {
  type Self = Insert
  override def self = this
  def left = table
  def right = linear
  override def childNames: Vector[String] = Vector("table " + tableSym, "linear")
  def generators = ConstArray((tableSym, table))
  def rebuild(l: Node, r: Node) = copy(table = l, linear = r)
  override def rebuildWithSymbols(gen: ConstArray[TermSymbol]): Insert = copy(tableSym = gen(0))
  def withInferredType(scope: Type.Scope, typeChildren: Boolean): Self = {
    val table2 = table.infer(scope, typeChildren)
    val lin2 = linear.infer(scope + (tableSym -> table2.nodeType), typeChildren)
    withChildren(ConstArray[Node](table2, lin2)) :@ (if (!hasType) lin2.nodeType else nodeType)
  }
  override def getDumpInfo = super.getDumpInfo.copy(mainInfo = allFields.mkString("allFields=[", ", ", "]"))
}

/** A column in an Insert operation. */
final case class InsertColumn(children: ConstArray[Node], fs: FieldSymbol, buildType: Type)
  extends Node
    with SimplyTypedNode {
  type Self = InsertColumn
  override def self = this
  protected[this] def rebuild(ch: ConstArray[Node]) = copy(children = ch)
  override def getDumpInfo = super.getDumpInfo.copy(mainInfo = fs.toString)
}
