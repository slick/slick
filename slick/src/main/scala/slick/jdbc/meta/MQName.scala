package slick.jdbc.meta

import java.sql.{DatabaseMetaData, ResultSet, SQLFeatureNotSupportedException}

import slick.basic.BasicStreamingAction
import slick.dbio.Effect
import slick.jdbc.{Invoker, JdbcBackend, PositionedResult, ResultSetInvoker, StreamingInvokerAction}
import slick.util.CloseableIterator

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

  /** The connection's current catalog and schema. The current schema is where the database creates objects named
    * without a schema; an unqualified name in SQL can also resolve elsewhere (e.g. on PostgreSQL, where it is the first
    * existing schema on the search_path and names resolve in the later ones too). Each is None (match any) when the
    * connection has no current one or the driver cannot report it. */
  private[meta] def currentCatalogAndSchema(session: JdbcBackend#JdbcSessionDef): (Option[String], Option[String]) = {
    val conn = session.conn
    val schema =
      try Option(conn.getSchema)
      catch { case _: AbstractMethodError | _: SQLFeatureNotSupportedException => None } // drivers before JDBC 4.1
    (Option(conn.getCatalog), schema)
  }

  /** `name` as a DatabaseMetaData search pattern that matches only `name`: `_`, `%` and the escape string are escaped
    * with the driver's search string escape. Unchanged when the driver has none (Derby), so it may match other names. */
  private[meta] def escapePattern(name: String, metaData: DatabaseMetaData): String = {
    val escape = metaData.getSearchStringEscape
    if (escape == null || escape.isEmpty) name
    else name.replace(escape, escape + escape).replace("_", escape + "_").replace("%", escape + "%")
  }

  /** Rows of a DatabaseMetaData lookup in the connection's current catalog and schema, which `lookup` gets as arguments.
    * Rows in other schemas are dropped: drivers that cannot escape a schema pattern (Derby), or that treat a schema
    * name argument as a pattern (Derby's getPrimaryKeys), also return the schemas that `_` or `%` in its name match. */
  private[meta] def lookupInCurrentSchema[R](
    lookup: (JdbcBackend#JdbcSessionDef, Option[String], Option[String]) => ResultSet)(
    conv: PositionedResult => R, schemaOf: R => Option[String]): BasicStreamingAction[Vector[R], R, Effect.Read] =
    new StreamingInvokerAction[Vector[R], R, Effect.Read] {
      protected[this] def createInvoker(sql: Iterable[String]) = new Invoker[R] {
        def iteratorTo(maxRows: Int)(implicit session: JdbcBackend#JdbcSessionDef) = {
          val (cat, schema) = currentCatalogAndSchema(session)
          val rows = ResultSetInvoker(lookup(_, cat, schema))(conv).iteratorTo(0)
          val inSchema = rows.filter(r => schema.forall(s => schemaOf(r).contains(s)))
          CloseableIterator.from(if (maxRows > 0) inSchema.take(maxRows) else inSchema, () => rows.close())
        }
      }
      protected[this] def createBuilder = Vector.newBuilder[R]
      def statements: Iterable[String] = Nil
    }
}
