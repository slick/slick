package slick.jdbc.meta

import java.sql.SQLFeatureNotSupportedException

import slick.jdbc.{JdbcBackend, PositionedResult}

/** A qualified name with an optional catalog and schema. */
case class MQName(catalog: Option[String], schema: Option[String], name: String) {
  override def toString = "MQName(" + catalog.map(_ + ".").getOrElse("") + schema.map(_ + ".").getOrElse("") + name + ")"

  def catalog_? = catalog.orNull
  def schema_? = schema.orNull
}

object MQName {
  private[meta] def from(r: PositionedResult) = MQName(r.<<, r.<<, r.<<)

  private[meta] def optionalFrom(r: PositionedResult) = {
    val cat = r.nextStringOption()
    val schema = r.nextStringOption()
    r.nextStringOption() map (MQName(cat, schema, _))
  }

  /** A name without a catalog and schema. In JDBC metadata lookups an empty catalog or schema only matches objects
    * that have none, so on most databases this does not find objects in the current catalog and schema. */
  def local(name: String) = MQName(Some(""), Some(""), name)

  /** The connection's current catalog and schema, where an unqualified name resolves in SQL. Each is None (match any)
    * when the connection has no current one or the driver cannot report it. */
  private[meta] def currentCatalogAndSchema(session: JdbcBackend#JdbcSessionDef): (Option[String], Option[String]) = {
    val conn = session.conn
    val schema =
      try Option(conn.getSchema)
      catch { case _: AbstractMethodError | _: SQLFeatureNotSupportedException => None } // drivers before JDBC 4.1
    (Option(conn.getCatalog), schema)
  }
}
