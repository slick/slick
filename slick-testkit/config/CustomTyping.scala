package scala.slick.config

import scala.slick.schema.QualifiedName
import scala.slick.typeproviders.TypeMapper
import scala.slick.typeproviders.TypeExtractor
import scala.reflect.api.Universe

object CustomTyping extends TypeMapper {
  import TypeMapper._

  sealed trait Bool {
    def isTrue: Boolean
  }
  case object True extends Bool {
    def isTrue = true
  }
  case object False extends Bool {
    def isTrue = false
  }

  implicit val boolTypeMapper = MappedColumnType.base[Bool, Int](
    { b =>
      if (b == True) 1 else 0
    }, { i =>
      if (i == 1) True else False
    })
  type SimpleA = Tuple2[Bool, String]
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

  override def columnType(universe: Universe)(name: QualifiedName): Option[universe.Type] = name.lastPart match {
    case "A1" => Some(universe.typeOf[Bool])
    case _ => super.columnType(universe)(name)
  }

}
