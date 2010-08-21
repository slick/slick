package com.novocode.squery.meta

import java.sql._
import com.novocode.squery.{ResultSetInvoker, UnitInvoker}
import com.novocode.squery.simple.Implicit._

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
    else ResultSetInvoker[MClientInfoProperty](s => m.invoke(s.metaData).asInstanceOf[ResultSet]) { r =>
      MClientInfoProperty(r<<, r<<, r<<, r<<)
    }
  }
}
