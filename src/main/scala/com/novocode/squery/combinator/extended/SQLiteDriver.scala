package com.novocode.squery.combinator.extended

import com.novocode.squery.combinator._
import com.novocode.squery.combinator.basic.{AbstractBasicTable, BasicColumnOption, BasicTypeMapperDelegates, BasicDDLBuilder}

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

  override def addTypeAndOptions(c: NamedColumn[_], sb: StringBuilder) {
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
      case BasicColumnOption.Default(v) => defaultLiteral = c.asInstanceOf[NamedColumn[Any]].typeMapper(SQLiteDriver).valueToSQLLiteral(v)
    }
    if(sqlType eq null) sqlType = mapTypeName(c.typeMapper(SQLiteDriver))
    sb append sqlType
    if(defaultLiteral ne null) sb append " DEFAULT " append defaultLiteral
    if(autoIncrement) sb append " PRIMARY KEY AUTOINCREMENT"
    else if(notNull) sb append " NOT NULL"
    else if(primaryKey) sb append " PRIMARY KEY"
  }
}
