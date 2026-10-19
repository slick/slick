package slick.jdbc.meta

import slick.dbio.Effect
import slick.jdbc.{JdbcBackend, ResultSetAction}
import slick.basic.BasicStreamingAction

/** A wrapper for a row in the ResultSet returned by DatabaseMetaData.getPrimaryKeys(). */
case class MPrimaryKey(table: MQName, column: String, keySeq: Short, pkName: Option[String])

object MPrimaryKey {
  def getPrimaryKeys(table: MQName): BasicStreamingAction[Vector[MPrimaryKey], MPrimaryKey, Effect.Read] =
    getPrimaryKeysOf(_ => table)
  /** Primary key of the named table in the connection's current catalog and schema */
  def getPrimaryKeys(table: String): BasicStreamingAction[Vector[MPrimaryKey], MPrimaryKey, Effect.Read] =
    getPrimaryKeysOf { s =>
      val (cat, schema) = MQName.currentCatalogAndSchema(s)
      MQName(cat, schema, table)
    }

  private def getPrimaryKeysOf(table: JdbcBackend#JdbcSessionDef => MQName) = ResultSetAction[MPrimaryKey] { s =>
      val t = table(s)
      s.metaData.getPrimaryKeys(t.catalog_?, t.schema_?, t.name)
    } { r =>
      MPrimaryKey(MQName.from(r), r.<<, r.<<, r.<<)
  }
}
