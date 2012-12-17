package scala.slick.ast

import scala.language.implicitConversions
import scala.slick.SlickException

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
  override def toString = "Coll"
}

object CollectionTypeConstructor {
  def default = new CollectionTypeConstructor
}

trait MappedScalaType extends Type {
  def baseType: Type
  def toMapped(v: Any): Any
  def toBase(v: Any): Any
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
trait TypedType[T] extends Type {
  def optionType: OptionTypedType[T] = new OptionTypedType[T](this)
}

trait BaseTypedType[T] extends TypedType[T]

class OptionTypedType[T](val elementType: TypedType[T]) extends TypedType[Option[T]] with OptionType

/** Mark a TypedType as eligible for numeric operators. */
trait NumericTypedType

object TypedType {
  @inline implicit def typedTypeToOptionTypedType[T](implicit t: TypedType[T]): OptionTypedType[T] = t.optionType
}

/** A basic type which must be provided by all drivers */
sealed class StaticType[T](name: String) extends BaseTypedType[T] {
  override def toString = "StaticType."+name
}

object StaticType {
  implicit object Boolean extends StaticType[Boolean]("Boolean")
  implicit object Char extends StaticType[Char]("Char")
  implicit object Int extends StaticType[Int]("Int") with NumericTypedType
  implicit object Long extends StaticType[Long]("Long") with NumericTypedType
  implicit object Null extends StaticType[Null]("Null")
  implicit object String extends StaticType[String]("String")
  implicit object Unit extends StaticType[Unit]("Unit")
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

class DefaultSymbolScope(val m: Map[Symbol, Type]) extends SymbolScope {
  def + (entry: (Symbol, Type)) = new DefaultSymbolScope(m + entry)
  def get(sym: Symbol): Option[Type] = m.get(sym)
  def withDefault(f: (Symbol => Type)) = new DefaultSymbolScope(m.withDefault(f))
}

object NoScope extends SymbolScope {
  def + (entry: (Symbol, Type)) = this
  def get(sym: Symbol): Option[Type] =
    throw new SlickException("Internal error: NoScope.get called")
  def withDefault(f: (Symbol => Type)) =
    throw new SlickException("Internal error: NoScope.withDefault called")
}
