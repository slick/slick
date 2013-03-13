package scala.slick.schema

import scala.collection.mutable.ArrayBuffer

case class Table(table: String, columns: List[Column], constraints: List[Constraint]) {
  val scalaName = Naming.tableSQLToModule(table)

  val caseClassName = Naming.moduleToCaseClass(scalaName)

  def primaryKeys: List[Column] = constraints.collectFirst { case pk: PrimaryKey => pk.fields }.getOrElse(Nil)

  def foreignKeys: List[ForeignKey] = constraints.collect { case fk: ForeignKey => fk }

  def indices: List[Index] = constraints.collect { case idx: Index => idx }
}
