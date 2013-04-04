package scala.slick.typeproviders

import scala.slick.schema.QualifiedName
import scala.reflect.api.Universe

trait TypeMapper {
  implicit val implicit_z = 4 // TODO remove me!

  def tableType(universe: Universe)(name: QualifiedName): Option[universe.Type] = None
  //  def tableExtractor[T <: TypeExtractor](name: QualifiedName): Option[T] = None
  def tableExtractor(universe: Universe)(name: QualifiedName): Option[universe.Type] = None
  //  def tableExtractor(name: QualifiedName): Option[TypeExtractor] = None
  //  def tableExtractor(name: QualifiedName): Option[TypeExtractor[_, _]] = None
  //  def tableExtractor(name: QualifiedName): Option[AnyRef] = None
  def columnType(universe: Universe)(name: QualifiedName): Option[universe.Type] = None
}

trait TypeExtractor[Elem, TupleElem] extends (TupleElem => Elem) {
  //abstract class TypeExtractor {
  //  type Elem
  //  type TupleElem
  //  def apply(_1: Int, _2: String, _3: String, _4: String, _5: String, _6: String): Elem // HACK! FIXME
  def apply(tuple: TupleElem): Elem
  def unapply(elem: Elem): Option[TupleElem]
  //  def unapply(elem: TypeExtractor#Elem): Option[TupleElem] = unapply(elem.asInstanceOf[Elem]) // generates double definition error after erasure
}

/*package object types {
  type TypeExtractorTyped[E, T] = TypeExtractor {
    type Elem = E
    type TupleElem = T
  }
}*/