package scala.slick.jdbc.meta

import scala.slick.jdbc.{PositionedResult, ResultSetInvoker}
import scala.slick.jdbc.GetResult.GetString

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
}
