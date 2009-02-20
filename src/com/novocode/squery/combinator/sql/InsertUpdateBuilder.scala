package com.novocode.squery.combinator.sql

import scala.collection.mutable.HashMap
import java.io.PrintWriter
import com.novocode.squery.combinator._

class InsertUpdateBuilder(val column: ConvertibleColumn[_]) {

  def buildInsert: String = {
    val cols = new StringBuilder
    val vals = new StringBuilder
    var table:String = null
    def f(c: Any): Unit = c match {
      case p:Projection[_] =>
        for(i <- 0 until p.productArity)
          f(p.productElement(i))
      case t:Table[_] => f(t.*)
      case n:NamedColumn[_] =>
        if(!cols.isEmpty) {
          cols append ","
          vals append ","
        }
        cols append n.name
        vals append '?'
        if(table eq null) table = n.table.tableName
        else if(table != n.table.tableName) throw new SQueryException("Inserts must all be to the same table")
      case _ => throw new SQueryException("Cannot use column "+c+" in INSERT statement")
    }
    f(column)
    if(table eq null) throw new SQueryException("No table to insert into")
    "INSERT INTO " + table + " (" + cols + ") VALUES (" + vals + ")"
  }
}
