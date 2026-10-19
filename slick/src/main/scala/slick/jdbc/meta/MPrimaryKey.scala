package slick.jdbc.meta

import slick.dbio.Effect
import slick.jdbc.{PositionedResult, ResultSetAction}
import slick.basic.BasicStreamingAction

/** A wrapper for a row in the ResultSet returned by DatabaseMetaData.getPrimaryKeys(). */
case class MPrimaryKey(table: MQName, column: String, keySeq: Short, pkName: Option[String])

object MPrimaryKey {
  def getPrimaryKeys(table: MQName): BasicStreamingAction[Vector[MPrimaryKey], MPrimaryKey, Effect.Read] =
    ResultSetAction[MPrimaryKey](_.metaData.getPrimaryKeys(table.catalog_?, table.schema_?, table.name))(fromRow)
  /** Primary key of the named table in the connection's current catalog and schema */
  def getPrimaryKeys(table: String): BasicStreamingAction[Vector[MPrimaryKey], MPrimaryKey, Effect.Read] =
    MQName.lookupInCurrentSchema[MPrimaryKey] { (s, cat, schema) =>
      s.metaData.getPrimaryKeys(cat.orNull, schema.orNull, table) // not a pattern, except on Derby
    }(fromRow, _.table.schema)

  private def fromRow(r: PositionedResult) = MPrimaryKey(MQName.from(r), r.<<, r.<<, r.<<)
}
