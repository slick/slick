package org.scalaquery.meta

import java.io.PrintWriter
import org.scalaquery.session.Session

/**
 * Generate Scala code from database meta-data.
 */
object CodeGen {

  def output(table: MTable, out: PrintWriter)(implicit session: Session) {
    val columns = table.getColumns.list
    val pkeys = table.getPrimaryKeys.mapResult(k => (k.column, k)).list.toMap
    if(!columns.isEmpty) {
      out.print("object "+mkScalaName(table.name.name)+" extends Table[")
      if(columns.tail.isEmpty) out.print(scalaTypeFor(columns.head))
      else out.print("(" + columns.map(c => scalaTypeFor(c)).mkString(", ") + ")")
      out.println("](\""+table.name.name+"\") {")
      for(c <- columns) output(c, pkeys.get(c.column), out)
      out.println("  def * = " + columns.map(c => mkScalaName(c.column, false)).mkString(" ~ "))
      out.println("}")
    }
  }

  def output(c: MColumn, pkey: Option[MPrimaryKey], out: PrintWriter)(implicit session: Session) {
    out.print("  def "+mkScalaName(c.column, false)+" = column["+scalaTypeFor(c)+"](\""+c.column+"\"")
    for(n <- c.sqlTypeName) {
      out.print(", O DBType \""+n+"")
      for(i <- c.columnSize ) out.print("("+i+")")
      out.print("\"")
    }
    if(c.isAutoInc.getOrElse(false)) out.print(", O AutoInc")
    for(k <- pkey) out.print(", O PrimaryKey")
    out.println(")")
  }

  def mkScalaName(s: String, capFirst:Boolean = true) = {
    val b = new StringBuilder
    var cap = capFirst
    for(c <- s) {
      if(c == '_') cap = true
      else {
        val allowed = if(b.isEmpty) c.isUnicodeIdentifierStart else c.isUnicodeIdentifierPart
        if(allowed) b append (if(cap) c.toUpper else c.toLower)
        cap = false
      }
    }
    b.toString
  }

  def scalaTypeFor(c: MColumn): String =
    if(c.nullable.getOrElse(true)) "Option[" + scalaTypeFor(c.sqlType) + "]" else scalaTypeFor(c.sqlType)

  def scalaTypeFor(sqlType: Int): String = {
    import java.sql.Types._
    sqlType match {
      case BIT | BOOLEAN => "Boolean"
      case TINYINT => "Byte"
      case SMALLINT => "Short"
      case INTEGER => "Int"
      case BIGINT => "BigInteger"
      case FLOAT => "Float"
      case REAL | DOUBLE => "Double"
      case NUMERIC | DECIMAL => "BigDecimal"
      case CHAR | VARCHAR | LONGVARCHAR => "String"
      case DATE => "java.sql.Date"
      case TIME => "java.sql.Time"
      case TIMESTAMP => "java.sql.Timestamp"
      case BINARY | VARBINARY | LONGVARBINARY | BLOB => "java.sql.Blob"
      case NULL => "Null"
      case CLOB => "java.sql.Clob"
      case _ => "AnyRef"
    }
  }
}
