package org.scalaquery.meta

import java.sql._
import org.scalaquery.{ResultSetInvoker, UnitInvoker}

/**
 * A wrapper for a row in the ResultSet returned by DatabaseMetaData.getClientInfoProperties().
 */
case class MClientInfoProperty(name: String, maxLen: Int, defaultValue: String, description: String)

object MClientInfoProperty {
  private[this] val m = try { classOf[DatabaseMetaData].getMethod("getClientInfoProperties") }
    catch { case _:NoSuchMethodException => null }

  def getClientInfoProperties: UnitInvoker[MClientInfoProperty] = {
    /* Regular version, requires Java 1.6: 
    ResultSetInvoker[MClientInfoProperty](_.metaData.getClientInfoProperties()) { r =>
      MClientInfoProperty(r<<, r<<, r<<, r<<)
    }*/
    if(m == null) UnitInvoker.empty
    else ResultSetInvoker[MClientInfoProperty](s => DatabaseMeta.invokeForRS(m, s.metaData)) { r =>
      MClientInfoProperty(r<<, r<<, r<<, r<<)
    }
  }
}
