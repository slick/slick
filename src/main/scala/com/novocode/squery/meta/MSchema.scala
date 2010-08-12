package com.novocode.squery.meta

import java.sql._
import com.novocode.squery.{ResultSetInvoker, UnitInvoker}
import com.novocode.squery.session._
import com.novocode.squery.simple.Implicit._

/**
 * A wrapper for a row in the ResultSet returned by DatabaseMetaData.getSchemas().
 */
case class MSchema(schema: String, catalog: Option[String]) {
  override def toString = "MSchema(" + catalog.map(_ + ".").getOrElse("") + schema + ")"
}

object MSchema {
  def getSchemas(catalog: Option[String], schemaPattern: Option[String]): UnitInvoker[MSchema] =
    ResultSetInvoker[MSchema](_.conn.getMetaData().getSchemas(catalog.getOrElse(null), schemaPattern.getOrElse(null))) { r =>
      MSchema(r.nextString, r.nextStringOption)
  }

  def getSchemas: UnitInvoker[MSchema] =
    ResultSetInvoker[MSchema](_.conn.getMetaData().getSchemas()) { r =>
      MSchema(r.nextString, r.nextStringOption)
  }
}
