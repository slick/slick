package slick.jdbc.meta

import slick.jdbc.ResultSetAction

/** A wrapper for a row in the ResultSet returned by DatabaseMetaData.getSchemas(). */
case class MSchema(schema: String, catalog: Option[String]) {
  override def toString = "MSchema(" + catalog.map(_ + ".").getOrElse("") + schema + ")"
}

object MSchema {
  def getSchemas(catalog: Option[String], schemaPattern: Option[String]) = {
    ResultSetAction[MSchema] { s =>
      try s.metaData.getSchemas(catalog.orNull, schemaPattern.orNull)
      catch { case _: AbstractMethodError => null }
    } { r => MSchema(r.<<, r.<<?) }
  }

  def getSchemas = ResultSetAction[MSchema](_.metaData.getSchemas()) { r => MSchema(r.<<, r.<<?) }
}
