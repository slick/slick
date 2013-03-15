package scala.slick.driver

import scala.slick.jdbc.UnitInvoker
import scala.slick.jdbc.meta.MTable
import scala.slick.jdbc.meta.MColumn

/**
 * Type Provider Component for each Jdbc Driver
 */
trait JdbcTypeProviderComponent { driver: JdbcDriver =>
  import scala.reflect.runtime.universe._

  /**
   * retrieves tables in database
   */
  def getTables: UnitInvoker[MTable] = MTable.getTables(Some(""), Some(""), None, None)

  /**
   * returns the type associated to a column
   */
  def scalaTypeForColumn(column: MColumn): Type =
    if (column.nullable.getOrElse(true))
      appliedType(typeOf[Option[_]], List(scalaTypeForSqlType(column.sqlType)))
    else
      scalaTypeForSqlType(column.sqlType)

  def scalaTypeForSqlType(sqlType: Int): Type = {
    import java.sql.Types._
    sqlType match {
      case BIT | BOOLEAN => typeOf[Boolean]
      case TINYINT => typeOf[Byte]
      case SMALLINT => typeOf[Short]
      case INTEGER => typeOf[Int]
      case BIGINT => typeOf[java.math.BigInteger]
      case FLOAT => typeOf[Float]
      case REAL | DOUBLE => typeOf[Double]
      case NUMERIC | DECIMAL => typeOf[java.math.BigInteger]
      case CHAR | VARCHAR | LONGVARCHAR => typeOf[String]
      case DATE => typeOf[java.sql.Date]
      case TIME => typeOf[java.sql.Time]
      case TIMESTAMP => typeOf[java.sql.Timestamp]
      case BINARY | VARBINARY | LONGVARBINARY | BLOB => typeOf[java.sql.Blob]
      case NULL => typeOf[Null]
      case CLOB => typeOf[java.sql.Clob]
      case _ => typeOf[AnyRef]
    }
  }

}
