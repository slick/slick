package scala.slick.jdbc

import java.sql.ResultSet

sealed abstract class ResultSetType(val intValue: Int) { self =>

  def apply[T](base: JdbcBackend#Session)(f: JdbcBackend#Session => T): T = f(base.forParameters(rsType = self))

  def apply[T](f: => T)(implicit base: JdbcBackend#Session): T = apply(base)(_.asDynamicSession(f))

  def withDefault(r: ResultSetType) = this
}

object ResultSetType {

  case object Auto extends ResultSetType(ResultSet.TYPE_FORWARD_ONLY) {
    override def withDefault(r: ResultSetType) = r
  }

  case object ForwardOnly extends ResultSetType(ResultSet.TYPE_FORWARD_ONLY)

  case object ScrollInsensitive extends ResultSetType(ResultSet.TYPE_SCROLL_INSENSITIVE)

  case object ScrollSensitive extends ResultSetType(ResultSet.TYPE_SCROLL_SENSITIVE)
}
