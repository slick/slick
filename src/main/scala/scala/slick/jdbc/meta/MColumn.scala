package scala.slick.jdbc.meta

import java.sql._
import scala.slick.jdbc.ResultSetInvoker
import scala.slick.driver.JdbcTypesComponent

/**
 * A wrapper for a row in the ResultSet returned by DatabaseMetaData.getColumns().
 */
case class MColumn(
  table: MQName, name: String, sqlType: Int, typeName: String,
  size: Option[Int], decimalDigits: Option[Int], numPrecRadix: Int, nullable: Option[Boolean], remarks: Option[String],
  columnDef: Option[String], charOctetLength: Int, ordinalPosition: Int, isNullable: Option[Boolean], scope: Option[MQName],
  sourceDataType: Option[Int], isAutoInc: Option[Boolean]) {

  def sqlTypeName = JdbcTypesComponent.typeNames.get(sqlType)
  def getColumnPrivileges = MColumnPrivilege.getColumnPrivileges(table, name)
}

object MColumn {
  def getColumns(tablePattern: MQName, columnPattern: String) = ResultSetInvoker[MColumn](
      _.metaData.getColumns(tablePattern.catalog_?, tablePattern.schema_?, tablePattern.name, columnPattern)) { r =>
      MColumn(MQName.from(r), r.<<, r.<<, r.<<, r.<<, r.skip.<<, r.<<, r.nextInt match {
          case DatabaseMetaData.columnNoNulls => Some(false)
          case DatabaseMetaData.columnNullable => Some(true)
          case _ => None
        }, r.<<, r.<<, r.skip.skip.<<, r.<<, DatabaseMeta.yesNoOpt(r),
        if(r.hasMoreColumns) MQName.optionalFrom(r) else None,
        r.<<?,
        if(r.hasMoreColumns) DatabaseMeta.yesNoOpt(r) else None)
  }
}
