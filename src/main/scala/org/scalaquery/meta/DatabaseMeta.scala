package org.scalaquery.meta

import java.sql._
import java.lang.reflect.{Method, InvocationTargetException}
import org.scalaquery.{ResultSetInvoker, UnitInvoker}
import org.scalaquery.session._
import org.scalaquery.simple.GetResult.GetString

/**
 * Accessor methods for various database meta data.
 */
object DatabaseMeta {

  def getCatalogs = ResultSetInvoker[String](_.metaData.getCatalogs())

  def getTableTypes = ResultSetInvoker[String](_.metaData.getTableTypes())

  private[meta] def yesNoOpt(r: PositionedResult) = if(r.hasMoreColumns) r.nextString match {
    case "YES" => Some(true)
    case "NO" => Some(false)
    case _ => None
  } else None

  private[meta] def invokeForRS(m: Method, target: AnyRef, args: AnyRef*): ResultSet = {
    try { m.invoke(target, args:_*).asInstanceOf[ResultSet] } catch { case t => 
      (t match {
        case i: InvocationTargetException => i.getCause()
        case other => other
      }) match {
        case a: AbstractMethodError => null
        case other => throw other
      }
    }
  }
}
