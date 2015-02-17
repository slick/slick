package slick.jdbc.meta

import slick.jdbc.ResultSetAction

/** A wrapper for a row in the ResultSet returned by DatabaseMetaData.getClientInfoProperties(). */
case class MClientInfoProperty(name: String, maxLen: Int, defaultValue: String, description: String)

object MClientInfoProperty {
  def getClientInfoProperties = {
    ResultSetAction[MClientInfoProperty]{ s =>
      try s.metaData.getClientInfoProperties()
      catch { case _: AbstractMethodError => null }
    } { r =>
      MClientInfoProperty(r.<<, r.<<, r.<<, r.<<)
    }
  }
}
