package scala.slick.ast

import scala.language.implicitConversions
import scala.slick.SlickException
import scala.collection.generic.CanBuildFrom
import scala.reflect.ClassTag

/** Super-trait for all types */
trait Type

object Type {
  def select(tpe: Type, sym: Symbol): Type = (tpe, sym) match {
    case (StructType(es), _) => es.find(x => x._1 == sym).map(_._2).
      getOrElse(throw new SlickException("No type for symbol "+sym+" found in "+tpe))
    case (ProductType(es), ElementSymbol(i)) if i <= es.length => es(i-1)
    case _ => throw new SlickException("No type for symbol "+sym+" found in "+tpe)
  }
}

case class StructType(elements: Seq[(Symbol, Type)]) extends Type {
  override def toString = "{" + elements.iterator.map{ case (s, t) => s + ": " + t }.mkString(", ") + "}"
  lazy val symbolToIndex: Map[Symbol, Int] =
    elements.zipWithIndex.map { case ((sym, _), idx) => (sym, idx) }(collection.breakOut)
}

trait OptionType extends Type {
  override def toString = "Option[" + elementType + "]"
  def elementType: Type
}

object OptionType {
  def apply(tpe: Type): OptionType = new OptionType {
    def elementType = tpe
  }
}

case class ProductType(elements: IndexedSeq[Type]) extends Type {
  override def toString = "(" + elements.mkString(", ") + ")"
}

case class CollectionType(cons: CollectionTypeConstructor, elementType: Type) extends Type {
  override def toString = cons + "[" + elementType + "]"
}

case class CollectionTypeConstructor(dummy: String = "") {
  def canBuildFrom = implicitly[CanBuildFrom[Vector[Any], Any, Vector[Any]]]
  override def toString = "Coll"
}

object CollectionTypeConstructor {
  def default = new CollectionTypeConstructor
}

trait MappedScalaType extends Type {
  def baseType: Type
  def toMapped(v: Any): Any
  def toBase(v: Any): Any
  override def toString = s"Mapped[$baseType]"
}

case object NoType extends Type

case object UnassignedType extends Type

/** Something that has a type */
trait Typed {
  def tpe: Type
}

object Typed {
  def unapply(t: Typed) = Some(t.tpe)
}

/* A Type that carries a Scala type argument */
trait TypedType[T] extends Type { self =>
  def optionType: OptionTypedType[T] = new OptionTypedType[T] {
    val elementType = self
    def scalaType = new ScalaOptionType[T](self.scalaType)
  }
  def scalaType: ScalaType[T]
}

trait BaseTypedType[T] extends TypedType[T]

trait OptionTypedType[T] extends TypedType[Option[T]] with OptionType {
  val elementType: TypedType[T]
}

/** Mark a TypedType as eligible for numeric operators. */
trait NumericTypedType

object TypedType {
  @inline implicit def typedTypeToOptionTypedType[T](implicit t: TypedType[T]): OptionTypedType[T] = t.optionType
}

class TypeUtil(val tpe: Type) extends AnyVal {
  def asCollectionType: CollectionType = tpe match {
    case c: CollectionType => c
    case _ => throw new SlickException("Expected a collection type, found "+tpe)
  }
  def asOptionType: OptionType = tpe match {
    case o: OptionType => o
    case _ => throw new SlickException("Expected an option type, found "+tpe)
  }
}

object TypeUtil {
  implicit def typeToTypeUtil(tpe: Type) = new TypeUtil(tpe)
}

trait SymbolScope {
  def + (entry: (Symbol, Type)): SymbolScope
  def get(sym: Symbol): Option[Type]
  def withDefault(f: (Symbol => Type)): SymbolScope
}

object SymbolScope {
  val empty = new DefaultSymbolScope(Map.empty)
}

class DefaultSymbolScope(val m: Map[Symbol, Type]) extends SymbolScope {
  def + (entry: (Symbol, Type)) = new DefaultSymbolScope(m + entry)
  def get(sym: Symbol): Option[Type] = m.get(sym)
  def withDefault(f: (Symbol => Type)) = new DefaultSymbolScope(m.withDefault(f))
}

/** A Slick Type encoding of plain Scala types.
  *
  * This is used by QueryInterpreter and MemoryDriver. Values stored in
  * HeapBackend columns are also expected to use these types.
  *
  * All drivers should support the following types which are used internally
  * by the lifted embedding and the query compiler: Boolean, Char, Int, Long,
  * Null, String, Unit. */
trait ScalaType[T] extends TypedType[T] {
  override def optionType: ScalaOptionType[T] = new ScalaOptionType[T](this)
  def nullable: Boolean
  def ordered: Boolean
  def zero: T
  def scalaOrderingFor(ord: Ordering): scala.math.Ordering[T]
  final def scalaType = this
}

class ScalaBaseType[T](val zero: T)(implicit val tag: ClassTag[T], val ordering: scala.math.Ordering[T]) extends ScalaType[T] with BaseTypedType[T] {
  override def toString = "ScalaType[" + tag.runtimeClass.getName + "]"
  def nullable = false
  def ordered = ordering ne null
  def scalaOrderingFor(ord: Ordering) = {
    if(ordering eq null) throw new SlickException("No ordering defined for "+this)
    val base = if(ord.direction == Ordering.Desc) ordering.reverse else ordering
    val nullsFirst = if(ord.nulls == Ordering.NullsFirst) -1 else 1
    new scala.math.Ordering[T] {
      def compare(x: T, y: T): Int = {
        if((x.asInstanceOf[AnyRef] eq null) && (y.asInstanceOf[AnyRef] eq null)) 0
        else if(x.asInstanceOf[AnyRef] eq null) nullsFirst
        else if(y.asInstanceOf[AnyRef] eq null) -nullsFirst
        else base.compare(x, y)
      }
    }
  }
  override def hashCode = tag.hashCode
  override def equals(o: Any) = o match {
    case t: ScalaBaseType[_] => tag == t.tag
    case _ => false
  }
}

object ScalaBaseType {
  implicit val booleanType = new ScalaBaseType[Boolean](false)
  implicit val bigDecimalType: ScalaNumericType[BigDecimal] = new ScalaNumericType[BigDecimal] {
    def fromDouble(v: Double) = BigDecimal(v)
  }
  implicit val byteType: ScalaNumericType[Byte] = new ScalaNumericType[Byte] {
    def fromDouble(v: Double) = v.toByte
  }
  implicit val charType = new ScalaBaseType[Char](' ')
  implicit val doubleType: ScalaNumericType[Double] = new ScalaNumericType[Double] {
    def fromDouble(v: Double) = v
  }
  implicit val floatType: ScalaNumericType[Float] = new ScalaNumericType[Float] {
    def fromDouble(v: Double) = v.toFloat
  }
  implicit val intType: ScalaNumericType[Int] = new ScalaNumericType[Int] {
    def fromDouble(v: Double) = v.toInt
  }
  implicit val longType: ScalaNumericType[Long] = new ScalaNumericType[Long] {
    def fromDouble(v: Double) = v.toLong
  }
  implicit val nullType = new ScalaBaseType[Null](null)
  implicit val shortType: ScalaNumericType[Short] = new ScalaNumericType[Short] {
    def fromDouble(v: Double) = v.toShort
  }
  implicit val stringType = new ScalaBaseType[String]("")
  implicit val unitType = new ScalaBaseType[Unit](())

  private[this] val all: Map[ClassTag[_], ScalaBaseType[_]] =
    Seq(booleanType, bigDecimalType, byteType, charType, doubleType,
      floatType, intType, longType, nullType, shortType, stringType,
      unitType).map(s => (s.tag, s)).toMap

  def apply[T](implicit tag: ClassTag[T], ord: scala.math.Ordering[T] = null): ScalaBaseType[T] =
    all.getOrElse(tag, new ScalaBaseType[T](null.asInstanceOf[T])).asInstanceOf[ScalaBaseType[T]]
}

abstract class ScalaNumericType[T](implicit tag: ClassTag[T], val numeric: Numeric[T])
  extends ScalaBaseType[T](numeric.zero)(tag, numeric) with NumericTypedType {
  def fromDouble(v: Double): T
  def toDouble(v: T) = numeric.toDouble(v)
}

class ScalaOptionType[T](val elementType: ScalaType[T]) extends ScalaType[Option[T]] with OptionTypedType[T] {
  override def toString = "ScalaOptionType[" + elementType + "]"
  def nullable = true
  def ordered = elementType.ordered
  def zero = None
  def scalaOrderingFor(ord: Ordering) = {
    val nullsFirst = if(ord.nulls == Ordering.NullsFirst) -1 else 1
    val base = elementType.scalaOrderingFor(ord)
    new scala.math.Ordering[Option[T]] {
      def compare(x: Option[T], y: Option[T]): Int = {
        if(x == None && y == None) 0
        else if(x == None) nullsFirst
        else if(y == None) -nullsFirst
        else base.compare(x.get, y.get)
      }
    }
  }
}
