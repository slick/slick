package scala.slick.schema

case class Column(name: String, tpe: scala.reflect.runtime.universe.Type) {
  val scalaName = Naming.columnSQLToField(name)
  override def toString = s"$name: $tpe"
}
