package slick.jdbc.meta

import slick.jdbc.{PositionedResult, ResultSetAction}
import slick.jdbc.GetResult.GetString

/**
 * Accessor methods for various database meta data.
 */
object DatabaseMeta {

  def getCatalogs = ResultSetAction[String](_.metaData.getCatalogs())

  def getTableTypes = ResultSetAction[String](_.metaData.getTableTypes())

  private[meta] def yesNoOpt(r: PositionedResult) = if(r.hasMoreColumns) r.nextString match {
    case "YES" => Some(true)
    case "NO" => Some(false)
    case _ => None
  } else None
}
