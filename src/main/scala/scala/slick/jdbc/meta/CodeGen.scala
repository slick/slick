package scala.slick.jdbc.meta

import java.io.PrintWriter
import scala.slick.jdbc.JdbcBackend

/**
 * Generate Scala code from database meta-data.
 */
object CodeGen {

  def outputCase(table: MTable, out: PrintWriter)(implicit session: JdbcBackend.Session) {
    val columns = table.getColumns.list
    val pkeys = table.getPrimaryKeys.mapResult(k => (k.column, k)).list.toMap

    if (!columns.isEmpty) {
      val tableName = table.name.name
      val scalaName = mkScalaName(tableName)
      out.println(mkCaseClass(scalaName, columns))

      val scalaTableName = f"$scalaName%sTable"
      out.println(s"""object $scalaTableName extends Table[$scalaName]("$tableName") {""")

      for (c <- columns) {
        out.print("  ")
        out.println(mkAttribute(c, pkeys.get(c.column), false))
      }

      val starString = columns.map(c => mkAttributeOptionName(c)).mkString(" ~ ")
      out.println(s"  def * = $starString <> ($scalaName, $scalaName.unapply _)")
      out.println("}")
    }
  }

  def output(table: MTable, out: PrintWriter)(implicit session: JdbcBackend.Session) {
    val columns = table.getColumns.list
    val pkeys = table.getPrimaryKeys.mapResult(k => (k.column, k)).list.toMap

    if (!columns.isEmpty) {
      val tableName = table.name.name
      val scalaName = mkScalaName(tableName)
      val colString = columns.map(c => scalaTypeFor(c)).mkString(", ")

      out.println(s"""object $scalaName extends Table[($colString)]("$tableName") {""")

      for (c <- columns) {
        out.print("  ")
        out.println(mkAttribute(c, pkeys.get(c.column)))
      }

      val starString = columns.map(c => mkScalaName(c.column, false)).mkString(" ~ ")
      out.println(s"  def * = $starString")
      out.println("}")
    }
  }

  def mkAttribute(c: MColumn, pkey: Option[MPrimaryKey], allowOption: Boolean = true)(implicit session: JdbcBackend.Session) = {
    val scalaName = mkScalaName(c.column, false)
    val scalaType = scalaTypeFor(c, allowOption)
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
    val params = (for (c <- columns) yield mkParam(c)).mkString(", ")

    s"""case class $scalaName (
	$params)
"""
  }

  def mkParam(c: MColumn, allowOption: Boolean = false): String = {
    val colName = mkScalaName(c.column, false)
    val colType = scalaTypeFor(c, allowOption)

    s"""$colName: $colType"""
  }

  def mkAttributeOptionName(c: MColumn) = {
    val scalaName = mkScalaName(c.column, false)
    if (c.isNullable.getOrElse(true)) s"$scalaName.?" else scalaName
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

  def scalaTypeFor(c: MColumn, allowOption: Boolean = true): String = {
    val scalaType = scalaTypeFor(c.sqlType)
    if (allowOption && c.nullable.getOrElse(true)) s"Option[$scalaType]" else scalaType
  }

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
