package com.novocode.squery.combinator.basic

import scala.collection.mutable.HashMap
import java.io.PrintWriter
import com.novocode.squery.SQueryException
import com.novocode.squery.combinator._

class BasicInsertBuilder(val column: ColumnBaseU) {

  def buildInsert: String = {
    val cols = new StringBuilder
    val vals = new StringBuilder
    var table:String = null
    def f(c: Any): Unit = c match {
      case p:Projection[_] =>
        for(i <- 0 until p.productArity)
          f(Node(p.productElement(i)))
      case t:Table[_] => f(Node(t.*))
      case n:NamedColumn[_] =>
        if(!cols.isEmpty) {
          cols append ","
          vals append ","
        }
        cols append n.name
        vals append '?'
        if(table eq null) table = n.table.asInstanceOf[Table[_]].tableName
        else if(table != n.table.asInstanceOf[Table[_]].tableName) throw new SQueryException("Inserts must all be to the same table")
      case _ => throw new SQueryException("Cannot use column "+c+" in INSERT statement")
    }
    f(Node(column))
    if(table eq null) throw new SQueryException("No table to insert into")
    "INSERT INTO " + table + " (" + cols + ") VALUES (" + vals + ")"
  }
}
