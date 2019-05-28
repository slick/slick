package slick.lifted

import slick.util.ConstArray

import scala.language.{implicitConversions, higherKinds}
import slick.ast._
import FunctionSymbolExtensionMethods._
import ScalaBaseType._
import slick.SlickException

/** Extension methods for all columns */
trait ColumnExtensionMethods[B1, P1] extends Any with ExtensionMethods[B1, P1] {
  def === [P2, R](e: Rep[P2])(implicit om: o#arg[B1, P2]#to[Boolean, R]) =
    om.column(Library.==, n, e.toNode)
  def =!= [P2, R](e: Rep[P2])(implicit om: o#arg[B1, P2]#to[Boolean, R]) =
    om.column(Library.Not, Library.==.typed(om.liftedType, n, e.toNode))

  def < [P2, R](e: Rep[P2])(implicit om: o#arg[B1, P2]#to[Boolean, R]) =
    om.column(Library.<, n, e.toNode)
  def <= [P2, R](e: Rep[P2])(implicit om: o#arg[B1, P2]#to[Boolean, R]) =
    om.column(Library.<=, n, e.toNode)
  def > [P2, R](e: Rep[P2])(implicit om: o#arg[B1, P2]#to[Boolean, R]) =
    om.column(Library.>, n, e.toNode)
  def >= [P2, R](e: Rep[P2])(implicit om: o#arg[B1, P2]#to[Boolean, R]) =
    om.column(Library.>=, n, e.toNode)

  def in[P2, R, C[_]](e: Query[Rep[P2], _, C])(implicit om: o#arg[B1, P2]#to[Boolean, R]) =
    om.column(Library.In, n, e.toNode)
  def inSet[R](seq: scala.collection.Traversable[B1])(implicit om: o#to[Boolean, R]) =
    if(seq.isEmpty) om(LiteralColumn(false))
    else om.column(Library.In, n, ProductNode(ConstArray.from(seq.map{ v => LiteralNode(implicitly[TypedType[B1]], v) })))
  def inSetBind[R](seq: scala.collection.Traversable[B1])(implicit om: o#to[Boolean, R]) =
    if(seq.isEmpty) om(LiteralColumn(false))
    else om.column(Library.In, n, ProductNode(ConstArray.from(seq.map(v => LiteralNode(implicitly[TypedType[B1]], v, vol = true)))))

  def between[P2, P3, R](start: Rep[P2], end: Rep[P3])(implicit om: o#arg[B1, P2]#arg[B1, P3]#to[Boolean, R]) =
    om.column(Library.Between, n, start.toNode, end.toNode)
  def ifNull[B2, P2, R](e: Rep[P2])(implicit om: o#arg[B2, P2]#to[Boolean, R]): Rep[P2] =
    Library.IfNull.column[P2](n, e.toNode)(tpe(e))
}