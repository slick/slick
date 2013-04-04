package scala.slick.typeproviders

import scala.slick.schema.QualifiedName
import scala.reflect.api.Universe

trait TypeMapper {
  def tableType(universe: Universe)(name: QualifiedName): Option[universe.Type] = None
  def tableExtractor(universe: Universe)(name: QualifiedName): Option[universe.Type] = None
  def columnType(universe: Universe)(name: QualifiedName): Option[universe.Type] = None
}

trait TypeExtractor[Elem, TupleElem] extends (TupleElem => Elem) {
  def apply(tuple: TupleElem): Elem
  def unapply(elem: Elem): Option[TupleElem]
}
