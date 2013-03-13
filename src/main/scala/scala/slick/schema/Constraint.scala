package scala.slick.schema

import scala.slick.lifted.ForeignKeyAction

sealed trait Constraint
case class ForeignKey(pkTable: Table, fkTable: Table, fields: List[(Column, Column)], updateRule: ForeignKeyAction, deleteRule: ForeignKeyAction) extends Constraint
case class PrimaryKey(fields: List[Column]) extends Constraint
case class Index(fields: List[Column]) extends Constraint
