package org.scalaquery.ql.basic

import scala.collection.mutable.HashMap
import java.io.PrintWriter
import org.scalaquery.SQueryException
import org.scalaquery.ql._
import org.scalaquery.util._

class BasicInsertBuilder(val column: Any, val profile: BasicProfile) {
  import profile.sqlUtils._

  def buildInsert: String = {
    val (schema, table, cols, vals) = buildParts
    
    val insertInto = schema.map(quoteIdentifier(_) + "." + quoteIdentifier(table)).getOrElse(quoteIdentifier(table))
    
    "INSERT INTO " + insertInto + " (" + cols + ") VALUES (" + vals + ")"
  }

  def buildInsert(query: Query[_, _]): SQLBuilder.Result = {
    val (schema, table, cols, _) = buildParts
    val b = new SQLBuilder

    b += "INSERT INTO "
    
    schema.foreach(b += quoteIdentifier(_) += '.')
    b += quoteIdentifier(table)

    b += " (" += cols.toString += ") "

    val qb = profile.createQueryBuilder(query, NamingContext())
    qb.buildSelect(b)
    b.build
  }

  protected def buildParts: (Option[String],String, StringBuilder, StringBuilder) = {
    val cols = new StringBuilder
    val vals = new StringBuilder
    var table:String = null
    var schema:Option[String] = None
    def f(c: Any): Unit = c match {
      case p:Projection[_] =>
        for(i <- 0 until p.productArity)
          f(Node(p.productElement(i)))
      case t:AbstractTable[_] => f(Node(t.*))
      case n:NamedColumn[_] =>
        if(table eq null) {
          table = n.table.asInstanceOf[AbstractTable[_]].tableName
          schema = n.table.asInstanceOf[AbstractTable[_]].schemaName
        }
        else if(table != n.table.asInstanceOf[AbstractTable[_]].tableName) 
          throw new SQueryException("Inserts must all be to the same table")
        appendNamedColumn(n, cols, vals)
      case _ => throw new SQueryException("Cannot use column "+c+" in INSERT statement")
    }
    f(Node(column))
    if(table eq null) 
      throw new SQueryException("No table to insert into")
    (schema, table, cols, vals)
  }

  protected def appendNamedColumn(n: NamedColumn[_], cols: StringBuilder, vals: StringBuilder) {
    if(!cols.isEmpty) {
      cols append ","
      vals append ","
    }
    cols append quoteIdentifier(n.name)
    vals append '?'
  }
}
