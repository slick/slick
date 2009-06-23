package com.novocode.squery.combinator.sql

import scala.collection.mutable.HashMap
import java.io.PrintWriter
import com.novocode.squery.combinator._

class DDLBuilder(table: Table[_]) {

  def buildCreateTable = {
    val b = new StringBuilder append "CREATE TABLE " append table.tableName append " ("
    var first = true
    def f(c: Any): Unit = c match {
      case p:Projection[_] =>
        for(i <- 0 until p.productArity)
          f(p.productElement(i))
      case t:Table[_] => f(t.*)
      case n:NamedColumn[_] =>
        if(first) first = false
        else b append ","
        b append n.name append ' ' append sqlTypeFor(n)
      case _ => throw new SQueryException("Cannot use column "+c+" in CREATE TABLE statement")
    }
    f(table)
    b append ")" toString
  }

  private[this] def sqlTypeFor(c: Column[_]) = {
    var s = c match {
      case _:BooleanColumn => "BOOLEAN"
      case _:IntColumn => "INT"
      case _:StringColumn => "VARCHAR"
      case _ => throw new SQueryException("No SQL type mapping for column type "+c.getClass.getName)
    }
    c match {
      case n: NamedColumn[_] =>
        var notNull = false
        var autoIncrement = false
        var defaultLiteral:String = null
        for(o <- n.options) o match {
          case ColumnOption.NotNull => notNull = true
          case ColumnOption.AutoInc => autoIncrement = true
          case ColumnOption.Default(v) => defaultLiteral = n.valueToSQLLiteral(v)
        }
        if(defaultLiteral ne null) s += " DEFAULT " + defaultLiteral
        if(notNull) s += " NOT NULL"
        if(autoIncrement) s += " AUTO_INCREMENT"
      case _ => ()
    }
    s
  }
}
