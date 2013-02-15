package scala.slick.lifted

import scala.language.implicitConversions
import scala.slick.ast._
import FunctionSymbolExtensionMethods._
import StaticType._
import scala.slick.SlickException

trait ExtensionMethods[B1, P1] extends Any {
  def c: Column[P1]
  @inline def n = Node(c)
  @inline implicit def p1Type = c.tpe
  implicit def b1Type = (c.tpe match {
    case o: OptionTypedType[_] => o.elementType
    case b => b
  }).asInstanceOf[TypedType[B1]]
  implicit def optionType = (c.tpe match {
    case o: OptionTypedType[_] => o
    case b => b.optionType
  }).asInstanceOf[TypedType[Option[B1]]]
  type o = OptionMapperDSL.arg[B1, P1]
}

/** Extension methods for all Columns and all primitive values that can be lifted to Columns */
final class AnyExtensionMethods(val n: Node) extends AnyVal {
  def asColumnOf[U : TypedType] = Library.Cast.column[U](n)
  def asColumnOfType[U : TypedType](typeName: String) =
    Library.Cast.column[U](n, LiteralNode(implicitly[TypedType[U]], typeName))
}

/** Extension methods for all Columns */
trait ColumnExtensionMethods[B1, P1] extends Any with ExtensionMethods[B1, P1] {
  val c: Column[P1]

  def isNull = Library.==.column[Boolean](n, LiteralNode(null))
  def isNotNull = Library.Not.column[Boolean](Library.==.typed[Boolean](n, LiteralNode(null)))

  def is[P2, R](e: Column[P2])(implicit om: o#arg[B1, P2]#to[Boolean, R]) =
    om(Library.==.column(n, Node(e)))
  def === [P2, R](e: Column[P2])(implicit om: o#arg[B1, P2]#to[Boolean, R]) =
    om(Library.==.column(n, Node(e)))
  def isNot[P2, R](e: Column[P2])(implicit om: o#arg[B1, P2]#to[Boolean, R]) =
    om(Library.Not.column(Library.==.typed[Boolean](n, Node(e))))
  @deprecated("Use =!= instead", "0.9.0")
  def != [P2, R](e: Column[P2])(implicit om: o#arg[B1, P2]#to[Boolean, R]) =
    om(Library.Not.column(Library.==.typed[Boolean](n, Node(e))))
  def =!= [P2, R](e: Column[P2])(implicit om: o#arg[B1, P2]#to[Boolean, R]) =
    om(Library.Not.column(Library.==.typed[Boolean](n, Node(e))))

  def < [P2, R](e: Column[P2])(implicit om: o#arg[B1, P2]#to[Boolean, R]) =
    om(Library.<.column(n, Node(e)))
  def <= [P2, R](e: Column[P2])(implicit om: o#arg[B1, P2]#to[Boolean, R]) =
    om(Library.<=.column(n, Node(e)))
  def > [P2, R](e: Column[P2])(implicit om: o#arg[B1, P2]#to[Boolean, R]) =
    om(Library.>.column(n, Node(e)))
  def >= [P2, R](e: Column[P2])(implicit om: o#arg[B1, P2]#to[Boolean, R]) =
    om(Library.>=.column(n, Node(e)))

  def in[P2, R](e: Query[Column[P2], _])(implicit om: o#arg[B1, P2]#to[Boolean, R]) =
    om(Library.In.column(n, Node(e)))
  def notIn[P2, R](e: Query[Column[P2], _])(implicit om: o#arg[B1, P2]#to[Boolean, R]) =
    om(Library.Not.column(Library.In.typed[Boolean](n, Node(e))))
  def inSet[R](seq: Traversable[B1])(implicit om: o#to[Boolean, R]) = om(
    if(seq.isEmpty) ConstColumn(false)
    else Library.In.column(n, ProductNode(seq.map{ v => LiteralNode(implicitly[TypedType[B1]], v) }.toSeq)))
  def inSetBind[R](seq: Traversable[B1])(implicit om: o#to[Boolean, R]) = om(
    if(seq.isEmpty) ConstColumn(false)
    else Library.In.column(n, ProductNode(seq.map(v => LiteralNode(implicitly[TypedType[B1]], v, vol = true)).toSeq)))

  def between[P2, P3, R](start: Column[P2], end: Column[P3])(implicit om: o#arg[B1, P2]#arg[B1, P3]#to[Boolean, R]) =
    om(Library.Between.column(n, Node(start), Node(end)))
  def ifNull[B2, P2, R](e: Column[P2])(implicit om: o#arg[B2, P2]#to[Boolean, R]): Column[P2] =
    Library.IfNull.column[P2](n, Node(e))(e.tpe)
}

final class PlainColumnExtensionMethods[P1](val c: Column[P1]) extends AnyVal with ColumnExtensionMethods[P1, P1] {
  def ? : Column[Option[P1]] = Column.forNode(OptionApply(Node(c)))(c.tpe.optionType)
}

final class OptionColumnExtensionMethods[B1](val c: Column[Option[B1]]) extends AnyVal with ColumnExtensionMethods[B1, Option[B1]] {
  def getOrElse(default: => B1): Column[B1] =
    Column.forNode[B1](GetOrElse(Node(c), () => default))(c.tpe.asInstanceOf[OptionType].elementType.asInstanceOf[TypedType[B1]])
  def get: Column[B1] =
    getOrElse { throw new SlickException("Read NULL value for column "+this) }
}

/** Extension methods for numeric Columns */
final class NumericColumnExtensionMethods[B1, P1](val c: Column[P1]) extends AnyVal with ExtensionMethods[B1, P1] {
  def + [P2, R](e: Column[P2])(implicit om: o#arg[B1, P2]#to[B1, R]) =
    om(Library.+.column[B1](n, Node(e)))
  def - [P2, R](e: Column[P2])(implicit om: o#arg[B1, P2]#to[B1, R]) =
    om(Library.-.column[B1](n, Node(e)))
  def * [P2, R](e: Column[P2])(implicit om: o#arg[B1, P2]#to[B1, R]) =
    om(Library.*.column[B1](n, Node(e)))
  def / [P2, R](e: Column[P2])(implicit om: o#arg[B1, P2]#to[B1, R]) =
    om(Library./.column[B1](n, Node(e)))
  def % [P2, R](e: Column[P2])(implicit om: o#arg[B1, P2]#to[B1, R]) =
    om(Library.%.column[B1](n, Node(e)))
  def abs = Library.Abs.column[P1](n)
  def ceil = Library.Ceiling.column[P1](n)
  def floor = Library.Floor.column[P1](n)
  def sign[R](implicit om: o#to[Int, R]) =
    om(Library.Sign.column[Int](n))
  def toDegrees = Library.Degrees.column[P1](n)
  def toRadians = Library.Radians.column[P1](n)
}

/** Extension methods for Column[Boolean] and Column[Option[Boolean]] */
final class BooleanColumnExtensionMethods[P1](val c: Column[P1]) extends AnyVal with ExtensionMethods[Boolean, P1] {
  def &&[P2, R](b: Column[P2])(implicit om: o#arg[Boolean, P2]#to[Boolean, R]) =
    om(Library.And.column(n, Node(b)))
  def ||[P2, R](b: Column[P2])(implicit om: o#arg[Boolean, P2]#to[Boolean, R]) =
    om(Library.Or.column(n, Node(b)))
  def unary_! = Library.Not.column[Boolean](n)
}

/** Extension methods for Column[String] and Column[Option[String]] */
final class StringColumnExtensionMethods[P1](val c: Column[P1]) extends AnyVal with ExtensionMethods[String, P1] {
  def length[R](implicit om: o#to[Int, R]) =
    om(Library.Length.column[Int](n))
  def like[P2, R](e: Column[P2], esc: Char = '\0')(implicit om: o#arg[String, P2]#to[Boolean, R]) = om(
    if(esc == '\0') Library.Like.column(n, Node(e))
    else Library.Like.column(n, Node(e), LiteralNode(esc)))
  def ++[P2, R](e: Column[P2])(implicit om: o#arg[String, P2]#to[String, R]) =
    om(Library.Concat.column[String](n, Node(e)))
  def startsWith[R](s: String)(implicit om: o#to[Boolean, R]) =
    om(Library.StartsWith.column(n, LiteralNode(s)))
  def endsWith[R](s: String)(implicit om: o#to[Boolean, R]) =
    om(Library.EndsWith.column(n, LiteralNode(s)))
  def toUpperCase = Library.UCase.column[P1](n)
  def toLowerCase = Library.LCase.column[P1](n)
  def ltrim = Library.LTrim.column[P1](n)
  def rtrim = Library.RTrim.column[P1](n)
  def trim = Library.Trim.column[P1](n)
}

/** Extension methods for Queries of a single Column */
final class SingleColumnQueryExtensionMethods[B1, P1](val q: Query[Column[P1], _]) extends AnyVal {
  type OptionTM =  TypedType[Option[B1]]
  def min(implicit tm: OptionTM) = Library.Min.column[Option[B1]](Node(q))
  def max(implicit tm: OptionTM) = Library.Max.column[Option[B1]](Node(q))
  def avg(implicit tm: OptionTM) = Library.Avg.column[Option[B1]](Node(q))
  def sum(implicit tm: OptionTM) = Library.Sum.column[Option[B1]](Node(q))
}

trait ExtensionMethodConversions {
  implicit def anyColumnExtensionMethods[B1 : BaseTypedType](c: Column[B1]) = new AnyExtensionMethods(Node(c))
  implicit def anyOptionColumnExtensionMethods[B1](c: Column[Option[B1]]) = new AnyExtensionMethods(Node(c))
  implicit def anyValueExtensionMethods[B1 : BaseTypedType](v: B1) = new AnyExtensionMethods(LiteralNode(implicitly[TypedType[B1]], v))
  implicit def anyOptionValueExtensionMethods[B1 : TypedType](v: Option[B1]) = new AnyExtensionMethods(LiteralNode(implicitly[TypedType[Option[B1]]], v))
  implicit def columnExtensionMethods[B1 : BaseTypedType](c: Column[B1]) = new PlainColumnExtensionMethods[B1](c)
  implicit def optionColumnExtensionMethods[B1](c: Column[Option[B1]]) = new OptionColumnExtensionMethods[B1](c)
  implicit def numericColumnExtensionMethods[B1](c: Column[B1])(implicit tm: BaseTypedType[B1] with NumericTypedType) = new NumericColumnExtensionMethods[B1, B1](c)
  implicit def numericOptionColumnExtensionMethods[B1](c: Column[Option[B1]])(implicit tm: BaseTypedType[B1] with NumericTypedType) = new NumericColumnExtensionMethods[B1, Option[B1]](c)
  implicit def stringColumnExtensionMethods(c: Column[String]) = new StringColumnExtensionMethods[String](c)
  implicit def stringOptionColumnExtensionMethods(c: Column[Option[String]]) = new StringColumnExtensionMethods[Option[String]](c)
  implicit def booleanColumnExtensionMethods(c: Column[Boolean]) = new BooleanColumnExtensionMethods[Boolean](c)
  implicit def booleanOptionColumnExtensionMethods(c: Column[Option[Boolean]]) = new BooleanColumnExtensionMethods[Option[Boolean]](c)

  implicit def singleColumnQueryExtensionMethods[B1 : BaseTypedType](q: Query[Column[B1], _]) = new SingleColumnQueryExtensionMethods[B1, B1](q)
  implicit def singleOptionColumnQueryExtensionMethods[B1](q: Query[Column[Option[B1]], _]) = new SingleColumnQueryExtensionMethods[B1, Option[B1]](q)
}
