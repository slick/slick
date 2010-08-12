package com.novocode.squery.meta

import java.sql._
import com.novocode.squery.{ResultSetInvoker, UnitInvoker}
import com.novocode.squery.session._
import com.novocode.squery.simple.Implicit._

/**
 * Accessor methods for various database meta data.
 */
object DatabaseMeta {

  def getCatalogs: UnitInvoker[String] =
    ResultSetInvoker[String]( _.conn.getMetaData().getCatalogs() ) { r => r.nextString }

  def getTableTypes: UnitInvoker[String] =
    ResultSetInvoker[String]( _.conn.getMetaData().getTableTypes() ) { r => r.nextString }

  private[meta] def yesNoOpt(r: PositionedResult) = r.nextString match {
    case "YES" => Some(true)
    case "NO" => Some(false)
    case _ => None
  }
}
