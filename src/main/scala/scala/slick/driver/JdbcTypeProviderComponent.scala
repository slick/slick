package scala.slick.driver

import scala.slick.jdbc.UnitInvoker
import scala.slick.jdbc.meta.MTable
import scala.slick.jdbc.meta.MColumn

trait JdbcTypeProviderComponent { driver: JdbcDriver =>
  def getTables: UnitInvoker[MTable] = MTable.getTables(Some(""), Some(""), None, None)
  
  def scalaTypeForColumn(column: MColumn): String =
    if(column.nullable.getOrElse(true)) "Option[" + scalaTypeForSqlType(column.sqlType) + "]" else scalaTypeForSqlType(column.sqlType)

  def scalaTypeForSqlType(sqlType: Int): String = {
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
