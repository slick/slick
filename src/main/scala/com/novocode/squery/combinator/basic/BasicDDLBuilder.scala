package com.novocode.squery.combinator.basic

import scala.collection.mutable.HashMap
import java.io.PrintWriter
import java.sql.Types._
import com.novocode.squery.SQueryException
import com.novocode.squery.combinator._

class BasicDDLBuilder(table: Table[_], profile: BasicProfile) {

  def buildCreateTable = {
    val b = new StringBuilder append "CREATE TABLE " append table.tableName append " ("
    var first = true
    for(n <- table.create_*) {
      if(first) first = false
      else b append ","
      b append n.name append ' '
      addTypeAndOptions(n, b)
    }
    b append ")" toString
  }

  protected def addTypeAndOptions(c: NamedColumn[_], sb: StringBuilder) {
    var sqlType: String = null
    var notNull = false
    var autoIncrement = false
    var primaryKey = false
    var defaultLiteral: String = null
    for(o <- c.options) o match {
      case ColumnOption.DBType(s) => sqlType = s
      case ColumnOption.NotNull => notNull = true
      case ColumnOption.AutoInc => autoIncrement = true
      case ColumnOption.PrimaryKey => primaryKey = true
      case ColumnOption.Default(v) => defaultLiteral = c.asInstanceOf[NamedColumn[Any]].typeMapper(profile).valueToSQLLiteral(v)
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
}
