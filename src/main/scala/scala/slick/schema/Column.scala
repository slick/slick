package scala.slick.schema

import scala.slick.schema.naming.Naming

case class Column(name: String, tpe: scala.reflect.runtime.universe.Type, moduleFieldName: String, caseFieldName: String) {
  override def toString = s"$name: $tpe"
}
