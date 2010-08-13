package com.novocode.squery.meta

import java.sql._
import com.novocode.squery.{ResultSetInvoker, UnitInvoker}
import com.novocode.squery.simple.Implicit._

/**
 * A wrapper for a row in the ResultSet returned by DatabaseMetaData.getClientInfoProperties().
 */
case class MClientInfoProperty(name: String, maxLen: Int, defaultValue: String, description: String)

object MClientInfoProperty {
  def getClientInfoProperties = ResultSetInvoker[MClientInfoProperty](_.metaData.getClientInfoProperties()) { r =>
      MClientInfoProperty(r<<, r<<, r<<, r<<)
  }
}
