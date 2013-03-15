package scala.slick.schema

import scala.collection.mutable.ArrayBuffer
import scala.slick.schema.naming.Naming

case class Table(table: String, columns: List[Column], constraints: List[Constraint], naming: Naming) {
  val scalaName = naming.tableSQLToModule(table)

  val caseClassName = naming.tableSQLToCase(table)

  def primaryKeys: List[Column] = constraints.collectFirst { case pk: PrimaryKey => pk.fields }.getOrElse(Nil)

  def foreignKeys: List[ForeignKey] = constraints.collect { case fk: ForeignKey => fk }

  def indices: List[Index] = constraints.collect { case idx: Index => idx }
}
