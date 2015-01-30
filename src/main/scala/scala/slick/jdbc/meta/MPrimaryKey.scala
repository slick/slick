package scala.slick.jdbc.meta

import scala.slick.dbio.Effect
import scala.slick.jdbc.{ResultSetAction, Invoker}
import scala.slick.profile.BasicStreamingAction

/** A wrapper for a row in the ResultSet returned by DatabaseMetaData.getPrimaryKeys(). */
case class MPrimaryKey(table: MQName, column: String, keySeq: Short, pkName: Option[String])

object MPrimaryKey {
  def getPrimaryKeys(table: MQName): BasicStreamingAction[Effect.Read, Vector[MPrimaryKey], MPrimaryKey] = ResultSetAction[MPrimaryKey](
      _.metaData.getPrimaryKeys(table.catalog_?, table.schema_?, table.name) ) { r =>
      MPrimaryKey(MQName.from(r), r.<<, r.<<, r.<<)
  }
  def getPrimaryKeys(table: String): BasicStreamingAction[Effect.Read, Vector[MPrimaryKey], MPrimaryKey] = getPrimaryKeys(MQName.local(table))
}
