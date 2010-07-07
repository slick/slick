package com.novocode.squery.combinator.basic

import scala.collection.mutable.HashMap
import java.io.PrintWriter
import com.novocode.squery.SQueryException
import com.novocode.squery.combinator._

class BasicInsertBuilder[T](val column: ColumnBase[T], val profile: BasicProfile) {

  def buildInsert: String = {
    val (table, cols, vals) = buildParts
    "INSERT INTO " + table + " (" + cols + ") VALUES (" + vals + ")"
  }

  def buildInsert(query: Query[ColumnBase[T]]): SQLBuilder.Result = {
    val (table, cols, _) = buildParts
    val b = new SQLBuilder
    b += "INSERT INTO " += table += " (" += cols.toString += ") "
    val qb = profile.createQueryBuilder(query, NamingContext())
    qb.buildSelect(b)
    b.build
  }

  protected def buildParts: (String, StringBuilder, StringBuilder) = {
    val cols = new StringBuilder
    val vals = new StringBuilder
    var table:String = null
    def f(c: Any): Unit = c match {
      case p:Projection[_] =>
        for(i <- 0 until p.productArity)
          f(Node(p.productElement(i)))
      case t:AbstractTable[_] => f(Node(t.*))
      case n:NamedColumn[_] =>
        if(!cols.isEmpty) {
          cols append ","
          vals append ","
        }
        cols append n.name
        vals append '?'
        if(table eq null) table = n.table.asInstanceOf[AbstractTable[_]].tableName
        else if(table != n.table.asInstanceOf[AbstractTable[_]].tableName) throw new SQueryException("Inserts must all be to the same table")
      case _ => throw new SQueryException("Cannot use column "+c+" in INSERT statement")
    }
    f(Node(column))
    if(table eq null) throw new SQueryException("No table to insert into")
    (table, cols, vals)
  }
}
