package scala.slick.typeproviders

import scala.slick.schema.QualifiedName
import scala.reflect.api.Universe
import scala.slick.jdbc.MappedJdbcType
import scala.slick.driver.JdbcDriver

trait TypeMapper {
  def tableType(universe: Universe)(name: QualifiedName): Option[universe.Type] = None
  def tableExtractor(universe: Universe)(name: QualifiedName): Option[universe.Type] = None
  def columnType(universe: Universe)(name: QualifiedName): Option[universe.Type] = None
  val MappedColumnType = MappedJdbcType
}

trait TypeExtractor[Elem, TupleElem] extends (TupleElem => Elem) {
  def apply(tuple: TupleElem): Elem
  def unapply(elem: Elem): Option[TupleElem]
}

object TypeMapper extends JdbcDriver.ImplicitJdbcTypes