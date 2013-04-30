package scala.slick.driver

import scala.slick.jdbc.UnitInvoker
import scala.slick.jdbc.meta.MTable
import scala.slick.jdbc.meta.MColumn
import scala.reflect.api.Universe

/**
 * Type Provider Component for each Jdbc Driver
 */
trait JdbcTypeProviderComponent { driver: JdbcDriver =>
  /**
   * retrieves tables in database
   */
  def getTables: UnitInvoker[MTable] = MTable.getTables(Some(""), Some(""), None, None)

  /**
   * returns the type associated to a column
   */
  def scalaTypeForColumn(universe: Universe)(column: MColumn): universe.Type =
    if (column.nullable.getOrElse(true))
      universe.appliedType(universe.typeOf[Option[_]], List(scalaTypeForSqlType(universe)(column.sqlType)))
    else
      scalaTypeForSqlType(universe)(column.sqlType)

  def scalaTypeForSqlType(universe: Universe)(sqlType: Int): universe.Type = {
    import java.sql.Types._
    import universe.typeOf
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
