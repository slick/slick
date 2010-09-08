package org.scalaquery.meta

import java.sql._
import org.scalaquery.{ResultSetInvoker, UnitInvoker}

/**
 * A wrapper for a row in the ResultSet returned by DatabaseMetaData.getIndexInfo().
 */
case class MIndexInfo(table: MQName, nonUnique: Boolean, indexQualifier: Option[String],
  indexName: Option[String], indexType: Short, ordinalPosition: Short,
  columnName: Option[String], ascending: Option[Boolean],
  cardinality: Int, pages: Int, filterCondition: Option[String])

object MIndexInfo {
  def getIndexInfo(table: MQName, unique: Boolean = false, approximate: Boolean = false) = ResultSetInvoker[MIndexInfo](
      _.metaData.getIndexInfo(table.catalog_?, table.schema_?, table.name, unique, approximate)) { r =>
      MIndexInfo(MQName.from(r), r<<, r<<, r<<, r<<, r<<, r<<, r.nextStringOption match {
          case Some("A") => Some(true)
          case Some("D") => Some(false)
          case _ => None
        }, r<<, r<<, r<<)
  }
}
