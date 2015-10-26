package slick.ast

import scala.language.{implicitConversions, higherKinds}
import slick.SlickException
import scala.collection.generic.CanBuild
import scala.collection.mutable.{Builder, ArrayBuilder}
import scala.reflect.{ClassTag, classTag => mkClassTag}
import Util._
import scala.collection.mutable.ArrayBuffer
import scala.annotation.implicitNotFound
import slick.util.{DumpInfo, Dumpable, TupleSupport, ConstArray}

/** Super-trait for all types */
trait Type extends Dumpable {
  /** All children of this Type. */
  def children: ConstArray[Type]
  /** Apply a transformation to all type children and reconstruct this
    * type with the new children, or return the original object if no
    * child is changed. */
  def mapChildren(f: Type => Type): Type
  /** Apply a side-effecting function to all children. */
  def childrenForeach[R](f: Type => R): Unit =
    children.foreach(f)
  def select(sym: TermSymbol): Type = throw new SlickException(s"No type for symbol $sym found in $this")
  /** The structural view of this type */
  def structural: Type = this
  /** Remove all NominalTypes recursively from this Type */
  def structuralRec: Type = structural.mapChildren(_.structuralRec)
  /** A ClassTag for the erased type of this type's Scala values */
  def classTag: ClassTag[_]
  def getDumpInfo = DumpInfo(DumpInfo.simpleNameFor(getClass), toString, "",
    children.zipWithIndex.map { case (ch, i) => (i.toString, ch) }.toSeq)
}

object Type {
  /** An extractor for strucural expansions of types */
  object Structural {
    def unapply(t: Type): Some[Type] = Some(t.structural)
  }

  type Scope = Map[TermSymbol, Type]
  def Scope(elems: (TermSymbol, Type)*): Scope = Map(elems: _*)
}

/** An atomic type (i.e. a type which does not contain other types) */
trait AtomicType extends Type {
  final def mapChildren(f: Type => Type): this.type = this
  def children = ConstArray.empty
  override final def childrenForeach[R](f: Type => R): Unit = ()
}

final case class StructType(elements: ConstArray[(TermSymbol, Type)]) extends Type {
  override def toString = "{" + elements.iterator.map{ case (s, t) => s + ": " + t }.mkString(", ") + "}"
  lazy val symbolToIndex: Map[TermSymbol, Int] =
    elements.zipWithIndex.map { case ((sym, _), idx) => (sym, idx) }.toMap
  def children: ConstArray[Type] = elements.map(_._2)
  def mapChildren(f: Type => Type): StructType = {
    val ch = elements.map(_._2)
    val ch2 = ch.endoMap(f)
    if(ch2 eq ch) this else StructType(elements.zip(ch2).map { case (e, t) => (e._1, t) })
  }
  override def select(sym: TermSymbol) = sym match {
    case ElementSymbol(idx) => elements(idx-1)._2
    case _ =>
      val i = elements.indexWhere(_._1 == sym)
      if(i >= 0) elements(i)._2 else super.select(sym)
  }
  def classTag = TupleSupport.classTagForArity(elements.length)
  override final def childrenForeach[R](f: Type => R): Unit = elements.foreach(t => f(t._2))
}

trait OptionType extends Type {
  override def toString = "Option[" + elementType + "]"
  def elementType: Type
  def children: ConstArray[Type] = ConstArray(elementType)
  def classTag = OptionType.classTag
  override def hashCode = elementType.hashCode() + 100
  override def equals(o: Any) = o match {
    case OptionType(elem) if elementType == elem => true
    case _ => false
  }
  override final def childrenForeach[R](f: Type => R): Unit = f(elementType)
}

object OptionType {
  def apply(tpe: Type): OptionType = tpe match {
    case t: TypedType[_] => t.optionType
    case _ =>
      new OptionType {
        def elementType = tpe
        def mapChildren(f: Type => Type): OptionType = {
          val e2 = f(elementType)
          if (e2 eq elementType) this
          else OptionType(e2)
        }
      }
  }
  def unapply(tpe: OptionType) = Some(tpe.elementType)
  private val classTag = mkClassTag[Option[_]]

  /** An extractor for a non-nested Option type of a single column */
  object Primitive {
    def unapply(tpe: Type): Option[Type] = tpe.structural match {
      case o: OptionType if o.elementType.structural.isInstanceOf[AtomicType] => Some(o.elementType)
      case _ => None
    }
  }

  /** An extractor for a nested or multi-column Option type */
  object NonPrimitive {
    def unapply(tpe: Type): Option[Type] = tpe.structural match {
      case o: OptionType if !o.elementType.structural.isInstanceOf[AtomicType] => Some(o.elementType)
      case _ => None
    }
  }
}

final case class ProductType(elements: ConstArray[Type]) extends Type {
  override def toString = "(" + elements.mkString(", ") + ")"
  def mapChildren(f: Type => Type): ProductType = {
    val ch2 = elements.endoMap(f)
    if(ch2 eq elements) this else ProductType(ch2)
  }
  override def select(sym: TermSymbol) = sym match {
    case ElementSymbol(i) if i <= elements.length => elements(i-1)
    case _ => super.select(sym)
  }
  def children: ConstArray[Type] = elements
  def classTag = TupleSupport.classTagForArity(elements.length)
}

final case class CollectionType(cons: CollectionTypeConstructor, elementType: Type) extends Type {
  override def toString = cons + "[" + elementType + "]"
  def mapChildren(f: Type => Type): CollectionType = {
    val e2 = f(elementType)
    if(e2 eq elementType) this
    else CollectionType(cons, e2)
  }
  override final def childrenForeach[R](f: Type => R): Unit = f(elementType)
  def children: ConstArray[Type] = ConstArray(elementType)
  def classTag = cons.classTag
}

/** Represents a type constructor that can be usd for a collection-valued query.
  * The relevant information for Slick is whether the elements of the collection
  * keep their insertion order (isSequential) and whether only distinct elements
  * are allowed (isUnique). */
trait CollectionTypeConstructor {
  /** The ClassTag for the type constructor */
  def classTag: ClassTag[_]
  /** Determines if order is relevant */
  def isSequential: Boolean
  /** Determines if only distinct elements are allowed */
  def isUnique: Boolean
  /** Create a `Builder` for the collection type, given a ClassTag for the element type */
  def createBuilder[E : ClassTag]: Builder[E, Any]
  /** Return a CollectionTypeConstructor which builds a subtype of Iterable
    * but has the same properties otherwise. */
  def iterableSubstitute: CollectionTypeConstructor =
    if(isUnique && !isSequential) TypedCollectionTypeConstructor.set
    else TypedCollectionTypeConstructor.seq
    //TODO We should have a better substitute for (isUnique && isSequential)
}

@implicitNotFound("Cannot use collection in a query\n            collection type: ${C}[_]\n  requires implicit of type: slick.ast.TypedCollectionTypeConstructor[${C}]")
abstract class TypedCollectionTypeConstructor[C[_]](val classTag: ClassTag[C[_]]) extends CollectionTypeConstructor {
  override def toString = classTag.runtimeClass.getName
    .replaceFirst("^scala.collection.immutable.", "")
    .replaceFirst("^scala.collection.mutable.", "m.")
    .replaceFirst("^scala.collection.generic.", "g.")
  def createBuilder[E : ClassTag]: Builder[E, C[E]]
  override def hashCode = classTag.hashCode() * 10
  override def equals(o: Any) = o match {
    case o: TypedCollectionTypeConstructor[_] => classTag == o.classTag
    case _ => false
  }
}

class ErasedCollectionTypeConstructor[C[_]](canBuildFrom: CanBuild[Any, C[Any]], classTag: ClassTag[C[_]]) extends TypedCollectionTypeConstructor[C](classTag) {
  val isSequential = classOf[scala.collection.Seq[_]].isAssignableFrom(classTag.runtimeClass)
  val isUnique = classOf[scala.collection.Set[_]].isAssignableFrom(classTag.runtimeClass)
  def createBuilder[E : ClassTag] = canBuildFrom().asInstanceOf[Builder[E, C[E]]]
}

object TypedCollectionTypeConstructor {
  private[this] val arrayClassTag = mkClassTag[Array[_]]
  /** The standard TypedCollectionTypeConstructor for Seq */
  def seq = forColl[Vector]
  /** The standard TypedCollectionTypeConstructor for Set */
  def set = forColl[Set]
  /** Get a TypedCollectionTypeConstructor for an Iterable type */
  implicit def forColl[C[X] <: Iterable[X]](implicit cbf: CanBuild[Any, C[Any]], tag: ClassTag[C[_]]): TypedCollectionTypeConstructor[C] =
    new ErasedCollectionTypeConstructor[C](cbf, tag)
  /** Get a TypedCollectionTypeConstructor for an Array type */
  implicit val forArray: TypedCollectionTypeConstructor[Array] = new TypedCollectionTypeConstructor[Array](arrayClassTag) {
    def isSequential = true
    def isUnique = false
    def createBuilder[E : ClassTag]: Builder[E, Array[E]] = ArrayBuilder.make[E]
  }
}

final class MappedScalaType(val baseType: Type, val mapper: MappedScalaType.Mapper, val classTag: ClassTag[_]) extends Type {
  override def toString = s"Mapped[$baseType]"
  def mapChildren(f: Type => Type): MappedScalaType = {
    val e2 = f(baseType)
    if(e2 eq baseType) this
    else new MappedScalaType(e2, mapper, classTag)
  }
  override final def childrenForeach[R](f: Type => R): Unit = f(baseType)
  def children: ConstArray[Type] = ConstArray(baseType)
  override def select(sym: TermSymbol) = baseType.select(sym)
  override def hashCode = baseType.hashCode() + mapper.hashCode() + classTag.hashCode()
  override def equals(o: Any) = o match {
    case o: MappedScalaType => baseType == o.baseType && mapper == o.mapper && classTag == o.classTag
    case _ => false
  }
}

object MappedScalaType {
  case class Mapper(toBase: Any => Any, toMapped: Any => Any, fastPath: Option[Any => Any])
}

/** The standard type for freshly constructed nodes without an explicit type. */
case object UnassignedType extends AtomicType {
  def classTag = throw new SlickException("UnassignedType does not have a ClassTag")
}

/** A type with a name, as used by tables.
 *
 * Compiler phases which change types may keep their own representation
 * of the structural view but must update the AST at the end of the phase
 * so that all NominalTypes with the same symbol have the same structural
 * view. */
final case class NominalType(sym: TypeSymbol, structuralView: Type) extends Type {
  override def toString = s"$sym<$structuralView>"
  def withStructuralView(t: Type): NominalType =
    if(t == structuralView) this else copy(structuralView = t)
  override def structural: Type = structuralView.structural
  override def select(sym: TermSymbol): Type = structuralView.select(sym)
  def mapChildren(f: Type => Type): NominalType = {
    val struct2 = f(structuralView)
    if(struct2 eq structuralView) this
    else new NominalType(sym, struct2)
  }
  override final def childrenForeach[R](f: Type => R): Unit = f(structuralView)
  def children: ConstArray[Type] = ConstArray(structuralView)
  def sourceNominalType: NominalType = structuralView match {
    case n: NominalType => n.sourceNominalType
    case _ => this
  }
  def classTag = structuralView.classTag
}

/** A Type that carries a Scala type argument */
trait TypedType[T] extends Type { self =>
  def optionType: OptionTypedType[T] = new OptionTypedType[T] {
    val elementType = self
    def scalaType = new ScalaOptionType[T](self.scalaType)
    def mapChildren(f: Type => Type): Type = {
      val e2 = f(elementType)
      if(e2 eq elementType) this
      else OptionType(e2)
    }
  }
  def scalaType: ScalaType[T]
}

trait BaseTypedType[T] extends TypedType[T] with AtomicType

trait OptionTypedType[T] extends TypedType[Option[T]] with OptionType {
  val elementType: TypedType[T]
}

/** Mark a TypedType as eligible for numeric operators. */
trait NumericTypedType

object TypedType {
  @inline implicit def typedTypeToOptionTypedType[T](implicit t: TypedType[T]): OptionTypedType[T] = t.optionType
}

class TypeUtil(val tpe: Type) extends AnyVal {
  import TypeUtil.typeToTypeUtil

  def asCollectionType: CollectionType = tpe match {
    case c: CollectionType => c
    case _ => throw new SlickException("Expected a collection type, found "+tpe)
  }
  def asOptionType: OptionType = tpe match {
    case o: OptionType => o
    case _ => throw new SlickException("Expected an option type, found "+tpe)
  }

  def replace(f: PartialFunction[Type, Type]): Type =
    f.applyOrElse(tpe, { case t: Type => t.mapChildren(_.replace(f)) }: PartialFunction[Type, Type])

  def collect[T](pf: PartialFunction[Type, T]): ConstArray[T] = {
    val retNull: (Type => T) = (_ => null.asInstanceOf[T])
    val b = ConstArray.newBuilder[T]()
    def f(n: Type): Unit = {
      val r = pf.applyOrElse(n, retNull)
      if(r.asInstanceOf[AnyRef] ne null) b += r
      n.childrenForeach(f)
    }
    f(tpe)
    b.result
  }

  def existsType(f: Type => Boolean): Boolean =
    if(f(tpe)) true else tpe match {
      case t: AtomicType => false
      case t => t.children.exists(_.existsType(f))
    }

  def containsSymbol(tss: scala.collection.Set[TypeSymbol]): Boolean =
    if(tss.isEmpty) false else tpe match {
      case NominalType(ts, exp) => tss.contains(ts) || exp.containsSymbol(tss)
      case t: AtomicType => false
      case t => t.children.exists(_.containsSymbol(tss))
    }
}

object TypeUtil {
  implicit def typeToTypeUtil(tpe: Type): TypeUtil = new TypeUtil(tpe)

  /** An extractor for node types */
  object :@ {
    def unapply(n: Node) = Some((n, n.nodeType))
  }
}

/** A Slick Type encoding of plain Scala types.
  *
  * This is used by QueryInterpreter and MemoryProfile. Values stored in
  * HeapBackend columns are also expected to use these types.
  *
  * All profiles should support the following types which are used internally
  * by the lifted embedding and the query compiler: Boolean, Char, Int, Long,
  * Null, String. */
trait ScalaType[T] extends TypedType[T] {
  override def optionType: ScalaOptionType[T] = new ScalaOptionType[T](this)
  def nullable: Boolean
  def ordered: Boolean
  def scalaOrderingFor(ord: Ordering): scala.math.Ordering[T]
  final def scalaType = this
  final def isPrimitive = classTag.runtimeClass.isPrimitive
}

class ScalaBaseType[T](implicit val classTag: ClassTag[T], val ordering: scala.math.Ordering[T]) extends ScalaType[T] with BaseTypedType[T] {
  override def toString = classTag.toString.replaceFirst("^java.lang.", "")
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
  override def hashCode = classTag.hashCode
  override def equals(o: Any) = o match {
    case t: ScalaBaseType[_] => classTag == t.classTag
    case _ => false
  }
}

class ErasedScalaBaseType[T, E](implicit val erasure: ScalaBaseType[E], val ct: ClassTag[T]) extends ScalaBaseType[T]()(ct, null) {
  override def toString = classTag.toString.replaceFirst("^slick.ast.", "") + "/" + erasure
}

object ScalaBaseType {
  implicit val booleanType = new ScalaBaseType[Boolean]
  implicit val bigDecimalType = new ScalaNumericType[BigDecimal](BigDecimal.apply _)
  implicit val byteType = new ScalaNumericType[Byte](_.toByte)
  implicit val charType = new ScalaBaseType[Char]
  implicit val doubleType = new ScalaNumericType[Double](identity)
  implicit val floatType = new ScalaNumericType[Float](_.toFloat)
  implicit val intType = new ScalaNumericType[Int](_.toInt)
  implicit val longType = new ScalaNumericType[Long](_.toLong)
  implicit val nullType = new ScalaBaseType[Null]
  implicit val shortType = new ScalaNumericType[Short](_.toShort)
  implicit val stringType = new ScalaBaseType[String]
  implicit val optionDiscType = new ErasedScalaBaseType[OptionDisc, Int]

  private[this] val all: Map[ClassTag[_], ScalaBaseType[_]] =
    Seq(booleanType, bigDecimalType, byteType, charType, doubleType,
      floatType, intType, longType, nullType, shortType, stringType,
      optionDiscType).map(s => (s.classTag, s)).toMap

  def apply[T](implicit classTag: ClassTag[T], ordering: scala.math.Ordering[T] = null): ScalaBaseType[T] =
    all.getOrElse(classTag, new ScalaBaseType[T]).asInstanceOf[ScalaBaseType[T]]

  def unapply[T](t: ScalaBaseType[T]) = Some((t.classTag,t.ordering))
}

/** A phantom type for Option discriminator columns. Values are of type Int. */
sealed trait OptionDisc

class ScalaNumericType[T](val fromDouble: Double => T)(implicit tag: ClassTag[T], val numeric: Numeric[T])
  extends ScalaBaseType[T]()(tag, numeric) with NumericTypedType {
  def toDouble(v: T) = numeric.toDouble(v)
}

class ScalaOptionType[T](val elementType: ScalaType[T]) extends ScalaType[Option[T]] with OptionTypedType[T] {
  override def toString = "SOption[" + elementType + "]"
  def nullable = true
  def ordered = elementType.ordered
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
  def mapChildren(f: Type => Type): ScalaOptionType[T] = {
    val e2 = f(elementType)
    if(e2 eq elementType) this
    else e2.asInstanceOf[ScalaType[T]].optionType
  }
}
