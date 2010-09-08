package org.scalaquery.meta

import java.sql._
import org.scalaquery.{ResultSetInvoker, UnitInvoker}
import org.scalaquery.ql.TypeMapperDelegate

/**
 * A wrapper for a row in the ResultSet returned by DatabaseMetaData.getVersionColumns().
 */
case class MVersionColumn(
  column: String, sqlType: Int, typeName: String,
  columnSize: Option[Int], bufferLength: Int, decimalDigits: Option[Int], pseudoColumn: Option[Boolean]) {

  def sqlTypeName = TypeMapperDelegate.typeNames.get(sqlType)
}

object MVersionColumn {
  def getVersionColumns(table: MQName) = ResultSetInvoker[MVersionColumn](
      _.metaData.getVersionColumns(table.catalog_?, table.schema_?, table.name) ) { r =>
      MVersionColumn(r.skip<<, r<<, r<<, r<<, r<<, r<<, r.nextInt match {
          case DatabaseMetaData.versionColumnPseudo => Some(true)
          case DatabaseMetaData.versionColumnNotPseudo => Some(false)
          case _ => None
        })
  }
}
