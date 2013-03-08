package scala.slick.typeProviders

import scala.slick.lifted.ForeignKeyAction
import scala.collection.mutable.ArrayBuffer

case class Schema(table: String, columns: List[Column]){
  import Column._
  
  val scalaName = Naming.tableSQLToModule(table)
  
  val caseClassName = Naming.moduleToCaseClass(scalaName)
  
  def primaryKeys: List[Column] = constraints.collect{case pk: Column.PrimaryKey => pk.fields}.headOption.getOrElse(Nil)
  
  def foreignKeys: List[ForeignKey] = constraints.collect{case fk: Column.ForeignKey => fk}
  
  def constraints: List[Constraint] = _constraints.toList
  
  private var _constraints: ArrayBuffer[Constraint] = new ArrayBuffer
  
  def +=(c:Constraint) {
    _constraints += c
  }
  
  def ++=(t: TraversableOnce[Constraint]) {
    _constraints ++= t
  }
}

case class Column(name: String, scalaType: String) {
  val scalaName = Naming.columnSQLToField(name)

  override def toString = s"$name: $scalaType"
  
  
}

object Column {
  sealed trait Constraint
  case class ForeignKey(pkTable: Schema, fkTable: Schema, fields: List[(Column, Column)], updateRule: ForeignKeyAction, deleteRule: ForeignKeyAction) extends Constraint
  case class PrimaryKey(fields: List[Column]) extends Constraint
}