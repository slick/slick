package slick.ast

import slick.util.ConstArray

/** Represents an Insert operation. */
final case class Insert(tableSym: TermSymbol, table: Node, linear: Node, allFields: ConstArray[FieldSymbol]) extends BinaryNode with DefNode {
  override type Self = Insert
  override def left = table
  override def right = linear
  //TODO 4.0.0: should we really expose that it is a Vector and not an Iterable as defined in Node? Changing it to Iterable is breaking
  override def childNames: Vector[String] = Vector("table "+tableSym, "linear")
  override def generators = ConstArray((tableSym, table))
  override def rebuild(l: Node, r: Node): Self = copy(table = l, linear = r)
  override def rebuildWithSymbols(gen: ConstArray[TermSymbol]) = copy(tableSym = gen(0))
  override def withInferredType(scope: Type.Scope, typeChildren: Boolean): Self = {
    val table2 = table.infer(scope, typeChildren)
    val lin2 = linear.infer(scope + (tableSym -> table2.nodeType), typeChildren)
    withChildren(ConstArray[Node](table2, lin2)) :@ (if(!hasType) lin2.nodeType else nodeType)
  }
  override def getDumpInfo = super.getDumpInfo.copy(mainInfo = allFields.mkString("allFields=[", ", ", "]"))
}

/** A column in an Insert operation. */
final case class InsertColumn(children: ConstArray[Node], fs: FieldSymbol, buildType: Type) extends Node with SimplyTypedNode {
  override type Self = InsertColumn
  override protected[this] def rebuild(ch: ConstArray[Node]): Self = copy(children = ch)
  override def getDumpInfo = super.getDumpInfo.copy(mainInfo = fs.toString)
}
