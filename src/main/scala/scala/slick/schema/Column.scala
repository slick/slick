package scala.slick.schema

import scala.slick.schema.naming.Naming

case class Column(tableName: String, name: String, tpe: scala.reflect.runtime.universe.Type, naming: Naming) {
  val scalaName = naming.columnSQLToCaseField(tableName)(name)
  override def toString = s"$name: $tpe"
}
