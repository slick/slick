package org.scalaquery.simple

import scala.collection.mutable.ArrayBuffer
import java.sql.{PreparedStatement, ResultSet, Date, Time, Timestamp}
import org.scalaquery.{StatementInvoker, UnitInvokerMixin, SQueryException}
import org.scalaquery.session.{PositionedParameters, PositionedResult}

/**
 * Base class for dynamic queries. These are required when the query text can
 * change between different invocations of the query
 */
abstract class DynamicQueryBase[T, +This <: DynamicQueryBase[T, This]] extends StatementInvoker[Unit, T] with UnitInvokerMixin[T] {
  self: This =>

  type VarSetter = PositionedParameters => Unit

  private val varSetters = new ArrayBuffer[VarSetter]
  private val buf: StringBuilder = new StringBuilder
  private lazy val query: String = buf.toString

  def ~(s: String) = { buf append s append ' '; this }

  def ~?(n: VarSetter): This = { buf append "? "; varSetters append n; this }
  def ~?(x: Boolean): This = ~?((pp: PositionedParameters) => pp.setBoolean(x))
  def ~?(x: Byte): This = ~?((pp: PositionedParameters) => pp.setByte(x))
  def ~?(x: Date): This = ~?((pp: PositionedParameters) => pp.setDate(x))
  def ~?(x: Double): This = ~?((pp: PositionedParameters) => pp.setDouble(x))
  def ~?(x: Float): This = ~?((pp: PositionedParameters) => pp.setFloat(x))
  def ~?(x: Int): This = ~?((pp: PositionedParameters) => pp.setInt(x))
  def ~?(x: Long): This = ~?((pp: PositionedParameters) => pp.setLong(x))
  def ~?(x: Short): This = ~?((pp: PositionedParameters) => pp.setShort(x))
  def ~?(x: String): This = ~?((pp: PositionedParameters) => pp.setString(x))
  def ~?(x: Time): This = ~?((pp: PositionedParameters) => pp.setTime(x))
  def ~?(x: Timestamp): This = ~?((pp: PositionedParameters) => pp.setTimestamp(x))

  def wrap(prefix: String, suffix: String)(body: => Unit) = {
    val pos = buf.size
    body
    if(buf.size != pos) {
      if(prefix != "") buf insert (pos, prefix + ' ')
      if(suffix != "") buf append suffix append ' '
    }
    this
  }

  override def toString = query

  protected def setParam(param: Unit, st: PreparedStatement) = {
    val pp = new PositionedParameters(st)
    for(s <- varSetters) s(pp)
  }

  protected def getStatement = query
}


class DynamicQuery[T](implicit rconv: GetResult[T]) extends DynamicQueryBase[T,DynamicQuery[T]] {
  def select = this ~ "select"
  def select(s: String) = this ~ "select" ~ s
  protected def extractValue(rs: PositionedResult): T = rconv(rs)
}

object DynamicQuery {
  def apply[T : GetResult] = new DynamicQuery[T]
}

class DynamicUpdate extends DynamicQueryBase[Int,DynamicUpdate] {
  def insert = this ~ "insert"
  def insert(s: String) = this ~ "insert" ~ s
  def update = this ~ "update"
  def update(s: String) = this ~ "update" ~ s
  protected def extractValue(rs: PositionedResult): Int =
    throw new SQueryException("DynamicUpdate.extractValue called; Non-query statements should not return a ResultSet")
}

object DynamicUpdate {
  def apply = new DynamicUpdate
}
