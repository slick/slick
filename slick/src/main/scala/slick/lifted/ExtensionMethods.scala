package slick.lifted

import slick.util.ConstArray

import scala.language.{implicitConversions, higherKinds}
import slick.ast._
import FunctionSymbolExtensionMethods._
import ScalaBaseType._
import slick.SlickException

trait ExtensionMethods[B1, P1] extends Any {
  protected[this] def c: Rep[P1]
  @inline protected[this] def n = c.toNode
  @inline protected[this] def tpe[T](r: Rep[T]): TypedType[T] = r.asInstanceOf[Rep.TypedRep[_]].tpe.asInstanceOf[TypedType[T]]
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
  def inSet[R](seq: Traversable[B1])(implicit om: o#to[Boolean, R]) =
    if(seq.isEmpty) om(LiteralColumn(false))
    else om.column(Library.In, n, ProductNode(ConstArray.from(seq.map{ v => LiteralNode(implicitly[TypedType[B1]], v) })))
  def inSetBind[R](seq: Traversable[B1])(implicit om: o#to[Boolean, R]) =
    if(seq.isEmpty) om(LiteralColumn(false))
    else om.column(Library.In, n, ProductNode(ConstArray.from(seq.map(v => LiteralNode(implicitly[TypedType[B1]], v, vol = true)))))

  def between[P2, P3, R](start: Rep[P2], end: Rep[P3])(implicit om: o#arg[B1, P2]#arg[B1, P3]#to[Boolean, R]) =
    om.column(Library.Between, n, start.toNode, end.toNode)
  def ifNull[B2, P2, R](e: Rep[P2])(implicit om: o#arg[B2, P2]#to[Boolean, R]): Rep[P2] =
    Library.IfNull.column[P2](n, e.toNode)(tpe(e))
}

final class BaseColumnExtensionMethods[P1](val c: Rep[P1]) extends AnyVal with ColumnExtensionMethods[P1, P1] with BaseExtensionMethods[P1] {
  /** Lift a column to an Option column. This is the same as calling [[slick.lifted.Rep.Some]]. */
  def ? : Rep[Option[P1]] = Rep.forNode(OptionApply(c.toNode))(p1Type.optionType)
}

final class OptionColumnExtensionMethods[B1](val c: Rep[Option[B1]]) extends AnyVal with ColumnExtensionMethods[B1, Option[B1]] with OptionExtensionMethods[B1] {
  /** Get the value inside this Option, if it is non-empty, otherwise throw a SlickException. This
    * operation is only allowed in places where it can be performed at the client side (e.g. not
    * inside a subquery that cannot be fused), otherwise the exception is thrown during query
    * compilation. */
  def get: Rep[B1] =
    Rep.forNode[B1](GetOrElse(c.toNode, () => throw new SlickException("Read NULL value for column " + this)))(c.asInstanceOf[Rep.TypedRep[_]].tpe.asInstanceOf[OptionType].elementType.asInstanceOf[TypedType[B1]])
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
  def unary_! = Library.Not.column[P1](n)
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
  def *[P1, R](i: Rep[P1])(implicit om: o#arg[Int, P1]#to[String, R]) =
    om.column(Library.Repeat, n, i.toNode)
}

/** Extension methods for all columns and all primitive values that can be lifted to columns */
final class AnyExtensionMethods(val n: Node) extends AnyVal {
  /** Cast a column to a Scala type. This can be used, for example, for numeric conversions. */
  def asColumnOf[U : TypedType] = Library.Cast.column[U](n)

  /** Cast a column to a database-specific SQL type (with an associated Scala type). */
  def asColumnOfType[U : TypedType](typeName: String) = Library.Cast.column[U](n, LiteralNode(typeName))
}

/** Extension methods for Queries of a single column */
final class SingleColumnQueryExtensionMethods[B1, P1, C[_]](val q: Query[Rep[P1], _, C]) extends AnyVal {
  type OptionTM =  TypedType[Option[B1]]

  /** Compute the minimum value of a single-column Query, or `None` if the Query is empty */
  def min(implicit tm: OptionTM) = Library.Min.column[Option[B1]](q.toNode)

  /** Compute the maximum value of a single-column Query, or `None` if the Query is empty */
  def max(implicit tm: OptionTM) = Library.Max.column[Option[B1]](q.toNode)

  /** Compute the average of a single-column Query, or `None` if the Query is empty */
  def avg(implicit tm: OptionTM) = Library.Avg.column[Option[B1]](q.toNode)

  /** Compute the sum of a single-column Query, or `None` if the Query is empty */
  def sum(implicit tm: OptionTM) = Library.Sum.column[Option[B1]](q.toNode)

  /** Count the number of `Some` elements of a single-column Query. */
  def countDefined(implicit ev: P1 <:< Option[_]) = Library.Count.column[Int](q.toNode)
}

/** Extension methods for Options of single- and multi-column values */
final class AnyOptionExtensionMethods[O <: Rep[_], P](val r: O) extends AnyVal {
  /** Apply `f` to the value inside this Option, if it is non-empty, otherwise return `ifEmpty`. */
  def fold[B, BP](ifEmpty: B)(f: P => B)(implicit shape: Shape[FlatShapeLevel, B, _, BP]): BP = {
    val gen = new AnonSymbol
    val mapv = f(OptionLift.baseValue[P, O](r, Ref(gen)))
    val n = OptionFold(r.toNode, shape.toNode(ifEmpty), shape.toNode(mapv), gen)
    shape.packedShape.encodeRef(shape.pack(mapv), n).asInstanceOf[BP]
  }

  /** Return the result of applying `f` to this Option's value if this Option is non-empty, otherwise None. */
  def flatMap[QO](f: P => Rep[Option[QO]]): Rep[Option[QO]] = {
    // This has to be implemented directly with a Rep wrapping an OptionFold node because we can't
    // create a None for arbitrary types. Here we can use a None of the dummy type Option[Null].
    val gen = new AnonSymbol
    val mapv = f(OptionLift.baseValue[P, O](r, Ref(gen)))
    mapv.encodeRef(OptionFold(r.toNode, LiteralNode.nullOption, mapv.toNode, gen))
  }

  /** Transform the value inside this Option */
  def map[Q, QO](f: P => Q)(implicit ol: OptionLift[Q, Rep[Option[QO]]]): Rep[Option[QO]] =
    flatMap[QO](p => Rep.Some(f(p)))

  /** Flatten a nested Option. */
  def flatten[QO](implicit ev: P <:< Rep[Option[QO]]): Rep[Option[QO]] =
    flatMap[QO](identity)

  /** Return this Option if it is non-empty and applying the predicate p to
    * this Option's value returns true. Otherwise, return None. */
  def filter[T](p: P => T)(implicit wt: CanBeQueryCondition[T]): O = {
    // fold(None, (v => if p(v) Some(v) else None))
    val gen = new AnonSymbol
    val pred = wt(p(OptionLift.baseValue[P, O](r, Ref(gen)))).toNode
    val cond = IfThenElse(ConstArray(pred, OptionApply(Ref(gen)), LiteralNode.nullOption))
    r.encodeRef(OptionFold(r.toNode, LiteralNode.nullOption, cond, gen)).asInstanceOf[O]
  }

  /** Get the value inside this Option, if it is non-empty, otherwise the supplied default. */
  def getOrElse[M, P2 <: P](default: M)(implicit shape: Shape[FlatShapeLevel, M, _, P2], ol: OptionLift[P2, O]): P =
    // P2 != P can only happen if M contains plain values, which pack to ConstColumn instead of Rep.
    // Both have the same packedShape (RepShape), so we can safely cast here:
    fold[P, P](shape.pack(default): P)(identity)(shape.packedShape.asInstanceOf[Shape[FlatShapeLevel, P, _, P]])

  /** Check if this Option is empty. */
  def isEmpty: Rep[Boolean] = fold(LiteralColumn(true))(_ => LiteralColumn(false))

  /** Check if this Option is non-empty. */
  def isDefined: Rep[Boolean] = fold(LiteralColumn(false))(_ => LiteralColumn(true))

  /** Check if this Option is non-empty. */
  def nonEmpty = isDefined
}

trait ExtensionMethodConversions {
  implicit def columnExtensionMethods[B1 : BaseTypedType](c: Rep[B1]): BaseColumnExtensionMethods[B1] = new BaseColumnExtensionMethods[B1](c)
  implicit def optionColumnExtensionMethods[B1 : BaseTypedType](c: Rep[Option[B1]]): OptionColumnExtensionMethods[B1] = new OptionColumnExtensionMethods[B1](c)
  implicit def numericColumnExtensionMethods[B1](c: Rep[B1])(implicit tm: BaseTypedType[B1] with NumericTypedType): BaseNumericColumnExtensionMethods[B1] = new BaseNumericColumnExtensionMethods[B1](c)
  implicit def numericOptionColumnExtensionMethods[B1](c: Rep[Option[B1]])(implicit tm: BaseTypedType[B1] with NumericTypedType): OptionNumericColumnExtensionMethods[B1] = new OptionNumericColumnExtensionMethods[B1](c)
  implicit def stringColumnExtensionMethods(c: Rep[String]): StringColumnExtensionMethods[String] = new StringColumnExtensionMethods[String](c)
  implicit def stringOptionColumnExtensionMethods(c: Rep[Option[String]]): StringColumnExtensionMethods[Option[String]] = new StringColumnExtensionMethods[Option[String]](c)
  implicit def mappedToStringColumnExtensionMethods[B1 <: MappedTo[String]](c: Rep[B1]): StringColumnExtensionMethods[String] = new StringColumnExtensionMethods[String](c.asInstanceOf[Rep[String]])
  implicit def mappedToOptionStringColumnExtensionMethods[B1 <: MappedTo[String]](c: Rep[Option[B1]]): StringColumnExtensionMethods[Option[String]] = new StringColumnExtensionMethods[Option[String]](c.asInstanceOf[Rep[Option[String]]])
  implicit def booleanColumnExtensionMethods(c: Rep[Boolean]): BooleanColumnExtensionMethods[Boolean] = new BooleanColumnExtensionMethods[Boolean](c)
  implicit def booleanOptionColumnExtensionMethods(c: Rep[Option[Boolean]]): BooleanColumnExtensionMethods[Option[Boolean]] = new BooleanColumnExtensionMethods[Option[Boolean]](c)

  implicit def anyColumnExtensionMethods[B1 : BaseTypedType](c: Rep[B1]): AnyExtensionMethods = new AnyExtensionMethods(c.toNode)
  implicit def anyOptionColumnExtensionMethods[B1 : BaseTypedType](c: Rep[Option[B1]]): AnyExtensionMethods = new AnyExtensionMethods(c.toNode)
  implicit def anyValueExtensionMethods[B1 : BaseTypedType](v: B1): AnyExtensionMethods = new AnyExtensionMethods(LiteralNode(implicitly[TypedType[B1]], v))
  implicit def anyOptionValueExtensionMethods[B1 : BaseTypedType](v: Option[B1]): AnyExtensionMethods = new AnyExtensionMethods(LiteralNode(implicitly[TypedType[Option[B1]]], v))
  implicit def singleColumnQueryExtensionMethods[B1 : BaseTypedType, C[_]](q: Query[Rep[B1], _, C]): SingleColumnQueryExtensionMethods[B1, B1, C] = new SingleColumnQueryExtensionMethods[B1, B1, C](q)
  implicit def singleOptionColumnQueryExtensionMethods[B1 : BaseTypedType, C[_]](q: Query[Rep[Option[B1]], _, C]): SingleColumnQueryExtensionMethods[B1, Option[B1], C] = new SingleColumnQueryExtensionMethods[B1, Option[B1], C](q)

  implicit def anyOptionExtensionMethods[T, P](v: Rep[Option[T]])(implicit ol: OptionLift[P, Rep[Option[T]]]): AnyOptionExtensionMethods[Rep[Option[T]], P] = new AnyOptionExtensionMethods[Rep[Option[T]], P](v)
}
