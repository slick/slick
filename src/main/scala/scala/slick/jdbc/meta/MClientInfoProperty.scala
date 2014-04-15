package scala.slick.jdbc.meta

import scala.slick.jdbc.ResultSetInvoker

/** A wrapper for a row in the ResultSet returned by DatabaseMetaData.getClientInfoProperties(). */
case class MClientInfoProperty(name: String, maxLen: Int, defaultValue: String, description: String)

object MClientInfoProperty {
  def getClientInfoProperties = {
    ResultSetInvoker[MClientInfoProperty]{ s =>
      try s.metaData.getClientInfoProperties()
      catch { case _: AbstractMethodError => null }
    } { r =>
      MClientInfoProperty(r.<<, r.<<, r.<<, r.<<)
    }
  }
}
