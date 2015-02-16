package slick.jdbc.meta

import slick.jdbc.ResultSetAction

/** A wrapper for a row in the ResultSet returned by DatabaseMetaData.getIndexInfo(). */
case class MIndexInfo(table: MQName, nonUnique: Boolean, indexQualifier: Option[String],
  indexName: Option[String], indexType: Short, ordinalPosition: Short,
  column: Option[String], ascending: Option[Boolean],
  cardinality: Int, pages: Int, filterCondition: Option[String])

object MIndexInfo {
  def getIndexInfo(table: MQName, unique: Boolean = false, approximate: Boolean = false) = ResultSetAction[MIndexInfo](
      _.metaData.getIndexInfo(table.catalog_?, table.schema_?, table.name, unique, approximate)) { r =>
      MIndexInfo(MQName.from(r), r.<<, r.<<, r.<<, r.<<, r.<<, r.<<, r.nextStringOption match {
          case Some("A") => Some(true)
          case Some("D") => Some(false)
          case _ => None
        }, r.<<, r.<<, r.<<)
  }
}
