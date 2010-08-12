package com.novocode.squery.meta

import java.sql._
import com.novocode.squery.{ResultSetInvoker, UnitInvoker}
import com.novocode.squery.session._
import com.novocode.squery.simple.Implicit._

/**
 * A wrapper for a row in the ResultSet returned by DatabaseMetaData.getClientInfoProperties().
 */
case class MClientInfoProperty(name: String, maxLen: Int, defaultValue: String, description: String)

object MClientInfoProperty {
  def getClientInfoProperties: UnitInvoker[MClientInfoProperty] =
    ResultSetInvoker[MClientInfoProperty](_.conn.getMetaData().getClientInfoProperties()) { r =>
      MClientInfoProperty(r.nextString, r.nextInt, r.nextString, r.nextString)
  }
}
