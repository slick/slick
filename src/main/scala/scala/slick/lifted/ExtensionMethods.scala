package scala.slick.lifted

import scala.language.{implicitConversions, higherKinds}
import scala.slick.ast._
import FunctionSymbolExtensionMethods._
import ScalaBaseType._
import scala.slick.SlickException

trait ExtensionMethods[B1, P1] extends Any {
  protected[this] def c: Rep[P1]
  @inline protected[this] def n = c.toNode
  @inline protected[this] def tpe[T](r: Rep[T]): TypedType[T] = r.asInstanceOf[Typed].tpe.asInstanceOf[TypedType[T]]
  @inline protected[this] implicit def p1Type = tpe(c)
  protected[this] implicit def b1Type: TypedType[B1]
  protected[this] type o = OptionMapperDSL.arg[B1, P1]
}

trait BaseExtensionMethods[B1] extends Any with ExtensionMethods[B1, B1] {
  protected[this] implicit def b1Type = p1Type
}

trait OptionExtensionMethods[B1] extends Any with ExtensionMethods[B1, Option[B1]] {
  protected[this] implicit def b1Type = p1Type.asInstanceOf[OptionType].elementType.asInstanceOf[TypedType[B1]]
}

/** Extension methods for all columns */
trait ColumnExtensionMethods[B1, P1] extends Any with ExtensionMethods[B1, P1] {
  @deprecated("Use 'isEmpty' instead of 'isNull'", "2.1")
  def isNull = Library.==.column[Boolean](n, LiteralNode(null))
  @deprecated("Use 'isDefined' instead of 'isNotNull'", "2.1")
  def isNotNull = Library.Not.column[Boolean](Library.==.typed[Boolean](n, LiteralNode(null)))

  def === [P2, R](e: Rep[P2])(implicit om: o#arg[B1, P2]#to[Boolean, R]) =
    om.column(Library.==, n, e.toNode)
  @deprecated("Use '===' instead of 'is'", "2.1")
  def is[P2, R](e: Rep[P2])(implicit om: o#arg[B1, P2]#to[Boolean, R]) =
    === [P2, R](e)
  @deprecated("Use '=!=' instead of 'isNot'", "2.1")
  def isNot[P2, R](e: Rep[P2])(implicit om: o#arg[B1, P2]#to[Boolean, R]) =
    =!= [P2, R](e)
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
  def inSet[R](seq: Traversable[B1])(implicit om: o#to[Boolean, R]) =
    if(seq.isEmpty) om(LiteralColumn(false))
    else om.column(Library.In, n, ProductNode(seq.map{ v => LiteralNode(implicitly[TypedType[B1]], v) }.toSeq))
  def inSetBind[R](seq: Traversable[B1])(implicit om: o#to[Boolean, R]) =
    if(seq.isEmpty) om(LiteralColumn(false))
    else om.column(Library.In, n, ProductNode(seq.map(v => LiteralNode(implicitly[TypedType[B1]], v, vol = true)).toSeq))

  def between[P2, P3, R](start: Rep[P2], end: Rep[P3])(implicit om: o#arg[B1, P2]#arg[B1, P3]#to[Boolean, R]) =
    om.column(Library.Between, n, start.toNode, end.toNode)
  def ifNull[B2, P2, R](e: Rep[P2])(implicit om: o#arg[B2, P2]#to[Boolean, R]): Rep[P2] =
    Library.IfNull.column[P2](n, e.toNode)(tpe(e))
}

final class BaseColumnExtensionMethods[P1](val c: Rep[P1]) extends AnyVal with ColumnExtensionMethods[P1, P1] with BaseExtensionMethods[P1] {
  def ? : Rep[Option[P1]] = Rep.forNode(OptionApply(c.toNode))(p1Type.optionType)
}

final class OptionColumnExtensionMethods[B1](val c: Rep[Option[B1]]) extends AnyVal with ColumnExtensionMethods[B1, Option[B1]] with OptionExtensionMethods[B1] {
  def getOrElse(default: => B1): Rep[B1] =
    Rep.forNode[B1](GetOrElse(c.toNode, () => default))(p1Type.asInstanceOf[OptionType].elementType.asInstanceOf[TypedType[B1]])
  def get: Rep[B1] =
    getOrElse { throw new SlickException("Read NULL value for column "+this) }
  /** Check if this Option column is empty (i.e. the underlying value is NULL) */
  def isEmpty = Library.==.column[Boolean](n, LiteralNode(null))
  /** Check if this Option column is not empty (i.e. the underlying value is not NULL) */
  def isDefined = Library.Not.column[Boolean](Library.==.typed[Boolean](n, LiteralNode(null)))
  /** Check if this Option column is not empty (i.e. the underlying value is not NULL) */
  def nonEmpty = isDefined
}

/** Extension methods for numeric columns */
trait NumericColumnExtensionMethods[B1, P1] extends Any with ExtensionMethods[B1, P1] {
  def + [P2, R](e: Rep[P2])(implicit om: o#arg[B1, P2]#to[B1, R]) =
    om.column(Library.+, n, e.toNode)
  def - [P2, R](e: Rep[P2])(implicit om: o#arg[B1, P2]#to[B1, R]) =
    om.column(Library.-, n, e.toNode)
  def * [P2, R](e: Rep[P2])(implicit om: o#arg[B1, P2]#to[B1, R]) =
    om.column(Library.*, n, e.toNode)
  def / [P2, R](e: Rep[P2])(implicit om: o#arg[B1, P2]#to[B1, R]) =
    om.column(Library./, n, e.toNode)
  def % [P2, R](e: Rep[P2])(implicit om: o#arg[B1, P2]#to[B1, R]) =
    om.column(Library.%, n, e.toNode)
  def abs = Library.Abs.column[P1](n)
  def ceil = Library.Ceiling.column[P1](n)
  def floor = Library.Floor.column[P1](n)
  def sign[R](implicit om: o#to[Int, R]) =
    om.column(Library.Sign, n)
  def toDegrees = Library.Degrees.column[P1](n)
  def toRadians = Library.Radians.column[P1](n)
}

final class BaseNumericColumnExtensionMethods[P1](val c: Rep[P1]) extends AnyVal with NumericColumnExtensionMethods[P1, P1] with BaseExtensionMethods[P1]

final class OptionNumericColumnExtensionMethods[B1](val c: Rep[Option[B1]]) extends AnyVal with NumericColumnExtensionMethods[B1, Option[B1]] with OptionExtensionMethods[B1]

/** Extension methods for Rep[Boolean] and Rep[Option[Boolean]] */
final class BooleanColumnExtensionMethods[P1](val c: Rep[P1]) extends AnyVal with ExtensionMethods[Boolean, P1] {
  protected[this] implicit def b1Type = implicitly[TypedType[Boolean]]

  def &&[P2, R](b: Rep[P2])(implicit om: o#arg[Boolean, P2]#to[Boolean, R]) =
    om.column(Library.And, n, b.toNode)
  def ||[P2, R](b: Rep[P2])(implicit om: o#arg[Boolean, P2]#to[Boolean, R]) =
    om.column(Library.Or, n, b.toNode)
  def unary_! = Library.Not.column[Boolean](n)
}

/** Extension methods for Rep[String] and Rep[Option[String]] */
final class StringColumnExtensionMethods[P1](val c: Rep[P1]) extends AnyVal with ExtensionMethods[String, P1] {
  protected[this] implicit def b1Type = implicitly[TypedType[String]]

  def length[R](implicit om: o#to[Int, R]) =
    om.column(Library.Length, n)
  def like[P2, R](e: Rep[P2], esc: Char = '\u0000')(implicit om: o#arg[String, P2]#to[Boolean, R]) =
    if(esc == '\u0000') om.column(Library.Like, n, e.toNode)
    else om.column(Library.Like, n, e.toNode, LiteralNode(esc))
  def ++[P2, R](e: Rep[P2])(implicit om: o#arg[String, P2]#to[String, R]) =
    om.column(Library.Concat, n, e.toNode)
  def startsWith[R](s: String)(implicit om: o#to[Boolean, R]) =
    om.column(Library.StartsWith, n, LiteralNode(s))
  def endsWith[R](s: String)(implicit om: o#to[Boolean, R]) =
    om.column(Library.EndsWith, n, LiteralNode(s))
  def toUpperCase = Library.UCase.column[P1](n)
  def toLowerCase = Library.LCase.column[P1](n)
  def ltrim = Library.LTrim.column[P1](n)
  def rtrim = Library.RTrim.column[P1](n)
  def trim = Library.Trim.column[P1](n)
  def reverseString = Library.Reverse.column[P1](n)
  def substring[P2, P3, R](start: Rep[P2], end: Rep[P3])(implicit om: o#arg[Int, P2]#arg[Int, P3]#to[String, R]) =
    om.column(Library.Substring, n, start.toNode, end.toNode)
  def substring[P2, R](start: Rep[P2])(implicit om: o#arg[Int, P2]#to[String, R]) =
    om.column(Library.Substring, n, start.toNode)
  def take[P2, R](num: Rep[P2])(implicit om: o#arg[Int, Int]#arg[Int, P2]#to[String, R]) =
    substring[Int, P2, R](LiteralColumn(0), num)
  def drop[P2, R](num: Rep[P2])(implicit om: o#arg[Int, P2]#to[String, R]) =
    substring[P2, R](num)
  def replace[P2, P3, R](target: Rep[P2], replacement: Rep[P3])(implicit om: o#arg[String, P2]#arg[String, P3]#to[String, R]) =
    om.column(Library.Replace, n, target.toNode, replacement.toNode)
  def indexOf[P2, R](str: Rep[P2])(implicit om: o#arg[String, P2]#to[Int, R]) =
    om.column(Library.IndexOf, n, str.toNode)
}

/** Extension methods for all columns and all primitive values that can be lifted to columns */
final class AnyExtensionMethods(val n: Node) extends AnyVal {
  def asColumnOf[U : TypedType] = Library.Cast.column[U](n)
  def asColumnOfType[U : TypedType](typeName: String) =
    Library.Cast.column[U](n, LiteralNode(implicitly[TypedType[U]], typeName))
}

/** Extension methods for Queries of a single column */
final class SingleColumnQueryExtensionMethods[B1, P1, C[_]](val q: Query[Rep[P1], _, C]) extends AnyVal {
  type OptionTM =  TypedType[Option[B1]]
  def min(implicit tm: OptionTM) = Library.Min.column[Option[B1]](q.toNode)
  def max(implicit tm: OptionTM) = Library.Max.column[Option[B1]](q.toNode)
  def avg(implicit tm: OptionTM) = Library.Avg.column[Option[B1]](q.toNode)
  def sum(implicit tm: OptionTM) = Library.Sum.column[Option[B1]](q.toNode)
}

trait ExtensionMethodConversions {
  implicit def columnExtensionMethods[B1 : BaseTypedType](c: Rep[B1]) = new BaseColumnExtensionMethods[B1](c)
  implicit def optionColumnExtensionMethods[B1 : BaseTypedType](c: Rep[Option[B1]]) = new OptionColumnExtensionMethods[B1](c)
  implicit def numericColumnExtensionMethods[B1](c: Rep[B1])(implicit tm: BaseTypedType[B1] with NumericTypedType) = new BaseNumericColumnExtensionMethods[B1](c)
  implicit def numericOptionColumnExtensionMethods[B1](c: Rep[Option[B1]])(implicit tm: BaseTypedType[B1] with NumericTypedType) = new OptionNumericColumnExtensionMethods[B1](c)
  implicit def stringColumnExtensionMethods(c: Rep[String]) = new StringColumnExtensionMethods[String](c)
  implicit def stringOptionColumnExtensionMethods(c: Rep[Option[String]]) = new StringColumnExtensionMethods[Option[String]](c)
  implicit def booleanColumnExtensionMethods(c: Rep[Boolean]) = new BooleanColumnExtensionMethods[Boolean](c)
  implicit def booleanOptionColumnExtensionMethods(c: Rep[Option[Boolean]]) = new BooleanColumnExtensionMethods[Option[Boolean]](c)

  implicit def anyColumnExtensionMethods[B1 : BaseTypedType](c: Rep[B1]) = new AnyExtensionMethods(c.toNode)
  implicit def anyOptionColumnExtensionMethods[B1 : BaseTypedType](c: Rep[Option[B1]]) = new AnyExtensionMethods(c.toNode)
  implicit def anyValueExtensionMethods[B1 : BaseTypedType](v: B1) = new AnyExtensionMethods(LiteralNode(implicitly[TypedType[B1]], v))
  implicit def anyOptionValueExtensionMethods[B1 : BaseTypedType](v: Option[B1]) = new AnyExtensionMethods(LiteralNode(implicitly[TypedType[Option[B1]]], v))
  implicit def singleColumnQueryExtensionMethods[B1 : BaseTypedType, C[_]](q: Query[Rep[B1], _, C]) = new SingleColumnQueryExtensionMethods[B1, B1, C](q)
  implicit def singleOptionColumnQueryExtensionMethods[B1 : BaseTypedType, C[_]](q: Query[Rep[Option[B1]], _, C]) = new SingleColumnQueryExtensionMethods[B1, Option[B1], C](q)
}
