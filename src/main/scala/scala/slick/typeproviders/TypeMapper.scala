package scala.slick.typeproviders

import scala.slick.schema.QualifiedName
import scala.reflect.api.Universe
import scala.slick.jdbc.MappedJdbcType
import scala.slick.driver.JdbcDriver

/**
 * Interface for specifying type mapping
 */
trait TypeMapper {
  def getType[T](implicit universe: Universe, ttag: scala.reflect.runtime.universe.TypeTag[T]): universe.Type = {
    scala.reflect.runtime.universe.typeOf[T].asInstanceOf[universe.Type]
  }
  def tableType(name: QualifiedName)(implicit universe: Universe): Option[universe.Type] = None
  def tableExtractor(name: QualifiedName)(implicit universe: Universe): Option[universe.Type] = None
  def columnType(name: QualifiedName)(implicit universe: Universe): Option[universe.Type] = None
}

/**
 * Interface for an extractor associated with each mapped type
 */
trait TypeExtractor[Elem, TupleElem] extends (TupleElem => Elem) {
  def apply(tuple: TupleElem): Elem
  def unapply(elem: Elem): Option[TupleElem]
}

object TypeMapper extends JdbcDriver.ImplicitJdbcTypes {
  val MappedColumnType = MappedJdbcType
}