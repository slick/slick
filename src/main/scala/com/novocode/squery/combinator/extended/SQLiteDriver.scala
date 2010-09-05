package com.novocode.squery.combinator.extended

import com.novocode.squery.combinator._
import com.novocode.squery.combinator.basic._
import com.novocode.squery.util._

object SQLiteDriver extends ExtendedProfile { self =>

  type ImplicitT = ExtendedImplicitConversions[SQLiteDriver.type]
  type TypeMapperDelegatesT = BasicTypeMapperDelegates

  val Implicit = new ExtendedImplicitConversions[SQLiteDriver.type] {
    implicit val squeryDriver = self
  }

  val typeMapperDelegates = new BasicTypeMapperDelegates {}

  override def buildTableDDL(table: AbstractBasicTable[_]): DDL = new SQLiteDDLBuilder(table).buildDDL
}

class SQLiteDDLBuilder(table: AbstractBasicTable[_]) extends BasicDDLBuilder(table, SQLiteDriver) {
  import profile.sqlUtils._

  protected class SQLiteColumnDDLBuilder(column: NamedColumn[_]) extends BasicColumnDDLBuilder(column) {
    override protected def appendOptions(sb: StringBuilder) {
      if(defaultLiteral ne null) sb append " DEFAULT " append defaultLiteral
      if(autoIncrement) sb append " PRIMARY KEY AUTOINCREMENT"
      else if(notNull) sb append " NOT NULL"
      else if(primaryKey) sb append " PRIMARY KEY"
    }
  }

  override protected def createColumnDDLBuilder(c: NamedColumn[_]) = new SQLiteColumnDDLBuilder(c)

  override def buildDDL: DDL = {
    val b = new StringBuilder append "CREATE TABLE " append table.tableName append " ("
    var first = true
    for(n <- table.create_*) {
      if(first) first = false
      else b append ","
      createColumnDDLBuilder(n).appendColumn(b)
    }
    for(fk <- table.foreignKeys) {
      b append ","
      addForeignKey(fk, b)
    }
    b append ")"
    new DDL {
      val createPhase1 = Iterable(b.toString)
      val createPhase2 = Iterable()
      val dropPhase1 = Nil
      val dropPhase2 = Iterable("DROP TABLE " + table.tableName)
    }
  }
}
