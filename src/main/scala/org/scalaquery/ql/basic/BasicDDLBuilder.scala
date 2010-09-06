package org.scalaquery.ql.basic

import scala.collection.mutable.HashMap
import java.io.PrintWriter
import org.scalaquery.SQueryException
import org.scalaquery.ql._
import org.scalaquery.ql.extended.ExtendedColumnOption //TODO: Move AutoInc handling to extended profile
import org.scalaquery.util.Node

class BasicDDLBuilder(val table: AbstractBasicTable[_], val profile: BasicProfile) {
  import profile.sqlUtils._

  protected class BasicColumnDDLBuilder(protected val column: NamedColumn[_]) {
    protected val tmDelegate = column.typeMapper(profile)
    protected var sqlType: String = null
    protected var notNull = !tmDelegate.nullable
    protected var autoIncrement = false
    protected var primaryKey = false
    protected var defaultLiteral: String = null
    init()

    protected def init() {
      for(o <- column.options) handleColumnOption(o)
      if(sqlType eq null) sqlType = mapTypeName(tmDelegate)
    }

    protected def handleColumnOption(o: ColumnOption[_,_]): Unit = o match {
      case BasicColumnOption.DBType(s) => sqlType = s
      case BasicColumnOption.NotNull => notNull = true
      case BasicColumnOption.Nullable => notNull = false
      case ExtendedColumnOption.AutoInc => autoIncrement = true
      case BasicColumnOption.PrimaryKey => primaryKey = true
      case BasicColumnOption.Default(v) => defaultLiteral = column.asInstanceOf[NamedColumn[Any]].typeMapper(profile).valueToSQLLiteral(v)
    }

    def appendColumn(sb: StringBuilder) {
      sb append quoteIdentifier(column.name) append ' '
      sb append sqlType
      appendOptions(sb)
    }

    protected def appendOptions(sb: StringBuilder) {
      if(defaultLiteral ne null) sb append " DEFAULT " append defaultLiteral
      if(notNull) sb append " NOT NULL"
      if(autoIncrement) sb append " AUTO_INCREMENT"
      if(primaryKey) sb append " PRIMARY KEY"
    }
  }

  protected def createColumnDDLBuilder(c: NamedColumn[_]) = new BasicColumnDDLBuilder(c)

  def buildDDL: DDL = {
    val createTable = {
      val b = new StringBuilder append "CREATE TABLE " append quoteIdentifier(table.tableName) append " ("
      var first = true
      for(n <- table.create_*) {
        if(first) first = false
        else b append ","
        createColumnDDLBuilder(n).appendColumn(b)
      }
      b append ")"
      b.toString
    }
    val createIndexes = table.indexes.map(createIndex)
    val alterTables1 = table.foreignKeys.map(createForeignKey)
    val alterTables2 = table.foreignKeys.map(dropForeignKey)
    new DDL {
      val createPhase1 = Iterable(createTable) ++ createIndexes
      val createPhase2 = alterTables1
      val dropPhase1 = alterTables2
      val dropPhase2 = Iterable("DROP TABLE " + quoteIdentifier(table.tableName))
    }
  }

  protected def createIndex(idx: Index) = {
    val b = new StringBuilder append "CREATE "
    if(idx.unique) b append "UNIQUE " 
    b append "INDEX " append quoteIdentifier(idx.name) append " ON " append quoteIdentifier(table.tableName) append "("
    addIndexColumnList(idx.on, b, idx.table.tableName)
    b append ")"
    b.toString
  }

  protected def createForeignKey(fk: ForeignKey[_ <: AbstractTable[_]]) = {
    val sb = new StringBuilder append "ALTER TABLE " append quoteIdentifier(table.tableName) append " ADD "
    addForeignKey(fk, sb)
    sb.toString
  }

  protected def addForeignKey(fk: ForeignKey[_ <: AbstractTable[_]], sb: StringBuilder) {
    sb append "CONSTRAINT " append quoteIdentifier(fk.name) append " FOREIGN KEY("
    addForeignKeyColumnList(fk.sourceColumns, sb, table.tableName)
    sb append ") REFERENCES " append quoteIdentifier(fk.targetTable.tableName) append "("
    addForeignKeyColumnList(fk.targetColumnsForOriginalTargetTable, sb, fk.targetTable.tableName)
    sb append ") ON UPDATE " append fk.onUpdate.action
    sb append " ON DELETE " append fk.onDelete.action
  }

  protected def dropForeignKey(fk: ForeignKey[_ <: AbstractTable[_]]) = {
    "ALTER TABLE " + quoteIdentifier(table.tableName) + " DROP CONSTRAINT " + quoteIdentifier(fk.name)
  }

  protected def addIndexColumnList(columns: Node, sb: StringBuilder, requiredTableName: String) =
    addColumnList(columns, sb, requiredTableName, "foreign key constraint")

  protected def addForeignKeyColumnList(columns: Node, sb: StringBuilder, requiredTableName: String) =
    addColumnList(columns, sb, requiredTableName, "index")

  protected def addColumnList(columns: Node, sb: StringBuilder, requiredTableName: String, typeInfo: String) = {
    var first = true
    def f(c: Any): Unit = c match {
      case p:Projection[_] =>
        for(i <- 0 until p.productArity)
          f(Node(p.productElement(i)))
      case t:AbstractTable[_] => f(Node(t.*))
      case n:NamedColumn[_] =>
        if(first) first = false
        else sb append ","
        sb append quoteIdentifier(n.name)
        if(requiredTableName != n.table.asInstanceOf[AbstractTable[_]].tableName)
          throw new SQueryException("All columns in "+typeInfo+" must belong to table "+requiredTableName)
      case _ => throw new SQueryException("Cannot use column "+c+
        " in "+typeInfo+" (only named columns and projections are allowed)")
    }
    f(Node(columns))
  }
}
