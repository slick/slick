package scala.slick.schema

import scala.slick.lifted.ForeignKeyAction

/**
 * A meta-model for constraints of a database
 */
sealed trait Constraint
case class ForeignKey(pkTableName: QualifiedName, fkTableName: QualifiedName, fields: List[(Column, Column)], updateRule: ForeignKeyAction, deleteRule: ForeignKeyAction) extends Constraint
case class PrimaryKey(fields: List[Column]) extends Constraint
case class Index(fields: List[Column]) extends Constraint
case class AutoIncrement(field: Column) extends Constraint