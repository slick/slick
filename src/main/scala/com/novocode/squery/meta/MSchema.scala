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
  private[this] val m = try { classOf[DatabaseMetaData].getMethod("getSchemas", classOf[String], classOf[String]) }
    catch { case _:NoSuchMethodException => null }

  def getSchemas(catalog: Option[String], schemaPattern: Option[String]) = {
    /* Regular version, requires Java 1.6: 
    ResultSetInvoker[MSchema](_.metaData.getSchemas(catalog.orNull, schemaPattern.orNull)) { r => MSchema(r<<, r<<?) }
    */
    if(m == null) UnitInvoker.empty
    else ResultSetInvoker[MSchema](s => 
      m.invoke(s.metaData, catalog.orNull, schemaPattern.orNull).asInstanceOf[ResultSet]) { r => MSchema(r<<, r<<?) }
  }

  def getSchemas = ResultSetInvoker[MSchema](_.metaData.getSchemas()) { r => MSchema(r<<, r<<?) }
}
