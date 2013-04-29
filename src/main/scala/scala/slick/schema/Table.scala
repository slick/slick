package scala.slick.schema

import scala.collection.mutable.ArrayBuffer
import scala.slick.schema.naming.Naming

/**
 * A meta-model for each relation
 */
case class Table(name: QualifiedName, columns: List[Column], constraints: List[Constraint], moduleName: String, caseClassName: String) {
  def primaryKey: Option[PrimaryKey] = constraints.collectFirst { case pk: PrimaryKey => pk }

  def foreignKeys: List[ForeignKey] = constraints.collect { case fk: ForeignKey => fk }

  def indices: List[Index] = constraints.collect { case idx: Index => idx }

  def autoInc: Option[AutoIncrement] = constraints.collectFirst { case ainc: AutoIncrement => ainc }
}
