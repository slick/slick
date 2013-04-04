package scala.slick.config

import scala.slick.schema.QualifiedName
import scala.slick.typeproviders.TypeMapper
import scala.slick.typeproviders.TypeExtractor
import scala.reflect.api.Universe

object CustomTyping extends TypeMapper {
  type SimpleA = Tuple2[Int, String]
  class SimpleAExtractor extends TypeExtractor[SimpleA, SimpleA] {
    override def unapply(s: SimpleA): Option[SimpleA] = Tuple2.unapply(s)
    def apply(s: SimpleA): SimpleA = s
  }

  override def tableType(universe: Universe)(name: QualifiedName): Option[universe.Type] = name.lastPart match {
    case "SIMPLE_AS" => Some(universe.typeOf[SimpleA])
    case _ => super.tableType(universe)(name)
  }

  override def tableExtractor(universe: Universe)(name: QualifiedName): Option[universe.Type] = name.lastPart match {
    case "SIMPLE_AS" => Some(universe.typeOf[SimpleAExtractor])
    case _ => super.tableExtractor(universe)(name)
  }

}
