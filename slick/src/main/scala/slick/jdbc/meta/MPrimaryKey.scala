package slick.jdbc.meta

import slick.dbio.Effect
import slick.jdbc.ResultSetAction
import slick.basic.BasicStreamingAction

/** A wrapper for a row in the ResultSet returned by DatabaseMetaData.getPrimaryKeys(). */
case class MPrimaryKey(table: MQName, column: String, keySeq: Short, pkName: Option[String])

object MPrimaryKey {
  def getPrimaryKeys(table: MQName): BasicStreamingAction[Vector[MPrimaryKey], MPrimaryKey, Effect.Read] = ResultSetAction[MPrimaryKey](
      _.metaData.getPrimaryKeys(table.catalog_?, table.schema_?, table.name) ) { r =>
      MPrimaryKey(MQName.from(r), r.<<, r.<<, r.<<)
  }
  def getPrimaryKeys(table: String): BasicStreamingAction[Vector[MPrimaryKey], MPrimaryKey, Effect.Read] = getPrimaryKeys(MQName.local(table))
}
