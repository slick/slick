package com.novocode.squery.combinator.basic

import scala.collection.mutable.HashMap
import java.io.PrintWriter
import java.sql.Types._
import com.novocode.squery.SQueryException
import com.novocode.squery.combinator._
import com.novocode.squery.combinator.extended.ExtendedColumnOption //TODO: Move AutoInc handling to extended profile

class BasicDDLBuilder(table: AbstractBasicTable[_], profile: BasicProfile) {

  def buildDDL: DDL = {
    val phase1 = {
      val b = new StringBuilder append "CREATE TABLE " append table.tableName append " ("
      var first = true
      for(n <- table.create_*) {
        if(first) first = false
        else b append ","
        b append n.name append ' '
        addTypeAndOptions(n, b)
      }
      b append ")"
      b.toString
    }
    val phase2 = table.foreignKeys.map { fk => 
      val b = new StringBuilder append "ALTER TABLE " append table.tableName append " ADD "
      addForeignKey(fk, b)
      b.toString
    }
    new DDL {
      val createPhase1 = Iterable(phase1)
      val createPhase2 = phase2
      val dropPhase1 = Nil
      val dropPhase2 = Iterable("DROP TABLE " + table.tableName)
    }
  }

  protected def addTypeAndOptions(c: NamedColumn[_], sb: StringBuilder) {
    var sqlType: String = null
    var notNull = false
    var autoIncrement = false
    var primaryKey = false
    var defaultLiteral: String = null
    for(o <- c.options) o match {
      case BasicColumnOption.DBType(s) => sqlType = s
      case BasicColumnOption.NotNull => notNull = true
      case ExtendedColumnOption.AutoInc => autoIncrement = true
      case BasicColumnOption.PrimaryKey => primaryKey = true
      case BasicColumnOption.Default(v) => defaultLiteral = c.asInstanceOf[NamedColumn[Any]].typeMapper(profile).valueToSQLLiteral(v)
    }
    if(sqlType eq null) sqlType = mapTypeName(c.typeMapper(profile))
    sb append sqlType
    if(defaultLiteral ne null) sb append " DEFAULT " append defaultLiteral
    if(notNull) sb append " NOT NULL"
    if(autoIncrement) sb append " AUTO_INCREMENT"
    if(primaryKey) sb append " PRIMARY KEY"
  }

  protected def mapTypeName(tmd: TypeMapperDelegate[_]): String = tmd.sqlType match {
    case VARCHAR => "VARCHAR(254)"
    case _ => tmd.sqlTypeName
  }

  protected def addForeignKey(fk: ForeignKey[_ <: AbstractTable[_]], sb: StringBuilder) {
    sb append "CONSTRAINT " append fk.name append " FOREIGN KEY("
    addForeignKeyColumnList(fk.sourceColumns, sb, table.tableName)
    sb append ") REFERENCES " append fk.targetTable.tableName append "("
    addForeignKeyColumnList(fk.targetColumnsForOriginalTargetTable, sb, fk.targetTable.tableName)
    sb append ") ON UPDATE " append fk.onUpdate.action
    sb append " ON DELETE " append fk.onDelete.action
  }

  protected def addForeignKeyColumnList(columns: Node, sb: StringBuilder, requiredTableName: String) = {
    var first = true
    def f(c: Any): Unit = c match {
      case p:Projection[_] =>
        for(i <- 0 until p.productArity)
          f(Node(p.productElement(i)))
      case t:AbstractTable[_] => f(Node(t.*))
      case n:NamedColumn[_] =>
        if(first) first = false
        else sb append ","
        sb append n.name
        if(requiredTableName != n.table.asInstanceOf[AbstractTable[_]].tableName)
          throw new SQueryException("All columns in foreign key constraint must belong to table "+requiredTableName)
      case _ => throw new SQueryException("Cannot use column "+c+
        " in foreign key constraint (only named columns and projections are allowed)")
    }
    f(Node(columns))
  }
}
