package com.novocode.squery.meta

import java.sql._
import com.novocode.squery.{ResultSetInvoker, UnitInvoker}
import com.novocode.squery.simple.Implicit._

/**
 * A wrapper for a row in the ResultSet returned by DatabaseMetaData.getSchemas().
 */
case class MSchema(schema: String, catalog: Option[String]) {
  override def toString = "MSchema(" + catalog.map(_ + ".").getOrElse("") + schema + ")"
}

object MSchema {
  def getSchemas(catalog: Option[String], schemaPattern: Option[String]) =
    ResultSetInvoker[MSchema](_.metaData.getSchemas(catalog.orNull, schemaPattern.orNull)) { r => MSchema(r<<, r<<) }

  def getSchemas = ResultSetInvoker[MSchema](_.metaData.getSchemas()) { r => MSchema(r<<, r<<) }
}
