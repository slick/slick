package com.novocode.squery.combinator.basic

import com.novocode.squery.combinator.{Query, NamingContext, Node, SQLBuilder, ColumnOps, TypeMapperDelegate, DDL}
import com.novocode.squery.combinator.basic._

object SQLiteDriver extends BasicProfile { self =>

  type ImplicitT = BasicImplicitConversions[SQLiteDriver.type]
  type TypeMapperDelegatesT = BasicTypeMapperDelegates

  val Implicit = new BasicImplicitConversions[SQLiteDriver.type] {
    implicit val squeryDriver = self
  }

  val typeMapperDelegates = new BasicTypeMapperDelegates {}

  override def buildTableDDL(table: AbstractBasicTable[_]): DDL = new SQLiteDDLBuilder(table).buildDDL
}

class SQLiteDDLBuilder(table: AbstractBasicTable[_]) extends BasicDDLBuilder(table, SQLiteDriver) {
  override def buildDDL: DDL = {
    val b = new StringBuilder append "CREATE TABLE " append table.tableName append " ("
    var first = true
    for(n <- table.create_*) {
      if(first) first = false
      else b append ","
      b append n.name append ' '
      addTypeAndOptions(n, b)
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
