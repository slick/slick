package slick.jdbc.meta

import java.sql._
import slick.jdbc.{ResultSetAction, JdbcTypesComponent}

/** A wrapper for a row in the ResultSet returned by DatabaseMetaData.getFunctionColumns(). */
case class MFunctionColumn(
  function: MQName, column: String, columnType: Short, sqlType: Int, typeName: String,
  precision: Option[Int], length: Int, scale: Option[Short], radix: Short,
  nullable: Option[Boolean], remarks: String, charOctetLength: Option[Int],
  ordinalPosition: Int, isNullable: Option[Boolean], specificName: String) {

  def sqlTypeName = JdbcTypesComponent.typeNames.get(sqlType)
}

object MFunctionColumn {
  def getFunctionColumns(functionPattern: MQName, columnNamePattern: String = "%") = {
    ResultSetAction[MFunctionColumn] { s =>
      try s.metaData.getFunctionColumns(functionPattern.catalog_?, functionPattern.schema_?,
                                       functionPattern.name, columnNamePattern)
      catch { case _: AbstractMethodError => null }
    } { r => MFunctionColumn(MQName.from(r), r.<<, r.<<, r.<<, r.<<, r.<<, r.<<, r.<<, r.<<, r.nextShort match {
        case DatabaseMetaData.functionNoNulls => Some(false)
        case DatabaseMetaData.functionNullable => Some(true)
        case _ => None
      }, r.<<, r.<<, r.<<, DatabaseMeta.yesNoOpt(r), r.<<)
    }
  }
}
