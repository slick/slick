package scala.slick.jdbc.meta

import java.io.PrintWriter
import java.lang.Math.pow


import scala.collection.immutable.List.apply
import scala.slick.jdbc.JdbcBackend

object Optional extends Enumeration {
  type Optional = Value
  val AllowOption, DenyOption = Value
}

/**
 * Generate Scala code from database meta-data.
 */
object CodeGen {
	import Optional._

  def outputCase(table: MTable, out: PrintWriter, colPicker: MColumn => Boolean = {c => true})(implicit session: JdbcBackend#Session) {
    val columns = table.getColumns.list filter colPicker
    val pkeys = table.getPrimaryKeys.mapResult(k => (k.column, k)).list.toMap

    if (!columns.isEmpty) {
      val tableName = table.name.name
      val scalaName = mkScalaName(tableName)
      out.println(mkCaseClass(scalaName, columns))

      val scalaTableName = s"${scalaName}Table"
      out.println(s"""object $scalaTableName extends Table[$scalaName]("$tableName") {""")

      for (c <- columns) {
        out.println(mkAttribute(c, pkeys.get(c.column), DenyOption))
      }

      val starString = columns.map(c => mkAttributeName(c, AllowOption)).mkString(" ~ ")
      out.println(s"  def * = $starString <> ($scalaName, $scalaName.unapply _)")
      out.println("}")
    }
  }

//  def output(table: MTable, out: PrintWriter)(implicit session: JdbcBackend#Session) =
//    outputTuple(table, out)(session)
	
  def output(table: MTable, out: PrintWriter, colPicker: MColumn => Boolean = {c => true})(implicit session: JdbcBackend#Session) {
    val columns = table.getColumns.list filter colPicker
    val pkeys = table.getPrimaryKeys.mapResult(k => (k.column, k)).list.toMap

    if (!columns.isEmpty) {
      val tableName = table.name.name
      val scalaName = mkScalaName(tableName)
      val colString = columns.map(c => scalaTypeFor(c, AllowOption)).mkString(", ")

      out.println(s"""object $scalaName extends Table[($colString)]("$tableName") {""")

      for (c <- columns) {
        out.println(mkAttribute(c, pkeys.get(c.column), AllowOption))
      }

      val starString = columns.map(c => mkAttributeName(c, DenyOption)).mkString(" ~ ")
      out.println(s"  def * = $starString")
      out.println("}")
    }
  }

  def mkAttribute(c: MColumn, pkey: Option[MPrimaryKey], optional: Optional)(implicit session: JdbcBackend#Session) = {
    val scalaName = mkScalaName(c.column, false)
    val scalaType = scalaTypeFor(c, optional)
    val columnName = c.column

    val columnParams = List(
      s""""$columnName"""",
      sqlTypeFor(c),
      if (!pkey.isEmpty) "O PrimaryKey" else null,
      if (c.isAutoInc.getOrElse(false)) "O AutoInc" else null
    ).filter(_ != null).mkString(", ")

    s"""  def $scalaName = column[$scalaType]($columnParams)"""
  }

  def mkCaseClass(scalaName: String, columns: Seq[MColumn]) = {
    val params = (for (c <- columns) yield mkParam(c, AllowOption)).mkString(", ")

    s"""case class $scalaName (
  $params)
"""
  }

  def mkParam(c: MColumn, optional: Optional): String = {
    val colName = mkScalaName(c.column, false)
    val colType = scalaTypeFor(c, optional)

    s"""$colName: $colType"""
  }

  def mkAttributeName(c: MColumn, optional: Optional = DenyOption) = {
    val scalaName = mkScalaName(c.column, false)
    if (optional == AllowOption && c.isNullable.getOrElse(true)) s"${scalaName}.?" else scalaName
  }

  def mkScalaName(s: String, capFirst: Boolean = true) = {
    val b = new StringBuilder
    var cap = capFirst
    for (c <- s) {
      if (c == '_') cap = true
      else {
        val allowed = if (b.isEmpty) c.isUnicodeIdentifierStart else c.isUnicodeIdentifierPart
        if (allowed) b append (if (cap) c.toUpper else c.toLower)
        cap = false
      }
    }
    b.toString
  }

  def sqlTypeFor(c: MColumn) = (c.sqlTypeName, c.columnSize) match {
    case (Some(n), None) => s"""O DBType "$n""""
    case (Some(n), Some(c)) => s"""O DBType "$n($c)""""
    case _ => ""
  }

  /**
   * @param optional if column is nullable return Option[Type] instead of [Type]
   */
  def scalaTypeFor(c: MColumn, optional: Optional): String = {
    import Optional._
    val scalaType = scalaTypeFor(c)
    if (optional == AllowOption && c.nullable.getOrElse(true)) s"Option[$scalaType]" else scalaType
  }

  def scalaTypeFor(c: MColumn): String = {
    import java.sql.Types._
    c.sqlType match {
      case BIT | BOOLEAN => "Boolean"
      case TINYINT => "Byte"
      case SMALLINT => "Short"
      case INTEGER => "Int"
      case BIGINT => "BigInteger"
      case FLOAT => "Float"
      case REAL | DOUBLE => "Double"
      case NUMERIC | DECIMAL => numericTypeFor(c.columnSize.getOrElse(0), c.decimalDigits.getOrElse(0))
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
  
  def numericTypeFor(columnSize: Int, decimalDigits: Int): String = {
    import Math._
    if (decimalDigits == 0) {
      val max = pow(10, columnSize)
      if ( max <= Int.MaxValue) "Int"
      else if (max <= Long.MaxValue) "Long"
      else "BigDecimal"
    }
    else {
      val digits = columnSize + decimalDigits
      //see http://en.wikipedia.org/wiki/IEEE_floating_point#Basic_formats
      if (digits <= 23) "Float"
      else if (digits <= 52) "Double"
      else "BigDecimal"
    }
  }
}
