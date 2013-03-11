package scala.slick.schema

import scala.collection.mutable.ArrayBuffer

case class Table(table: String, columns: List[Column]) {
  val scalaName = Naming.tableSQLToModule(table)

  val caseClassName = Naming.moduleToCaseClass(scalaName)

  def primaryKeys: List[Column] = constraints.collectFirst { case pk: PrimaryKey => pk.fields }.getOrElse(Nil)

  def foreignKeys: List[ForeignKey] = constraints.collect { case fk: ForeignKey => fk }

  def constraints: List[Constraint] = _constraints.toList

  private var _constraints: ArrayBuffer[Constraint] = new ArrayBuffer

  def +=(c: Constraint) {
    _constraints += c
  }

  def ++=(t: TraversableOnce[Constraint]) {
    _constraints ++= t
  }
}