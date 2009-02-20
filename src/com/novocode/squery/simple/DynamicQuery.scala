package com.novocode.squery.simple

import java.sql.{PreparedStatement, ResultSet, Date, Time, Timestamp}
import com.novocode.squery.session.PositionedResult

/**
 * Base class for dynamic queries. These are required when the query text can
 * change between different invocations of the query
 */
abstract class DynamicQueryBase[+T, +This <: DynamicQueryBase[T, This]] extends StatementInvoker[Unit, T] with NoArgsInvoker[T] {
  self: This =>

  type VarSetter = (PreparedStatement, Int) => Unit

  private var varSetters = Nil:List[VarSetter]
  private var varSettersLen = 0
  private val buf: StringBuilder = new StringBuilder
  private lazy val query: String = buf.toString

  def ~(s: String) = { buf append s append ' '; this }

  def ~?(n: VarSetter): This = { buf append "? "; varSetters = n :: varSetters; varSettersLen += 1; this }
  def ~?(x: Boolean): This = ~?((st: PreparedStatement, i: Int) => st.setBoolean(i, x))
  def ~?(x: Byte): This = ~?((st: PreparedStatement, i: Int) => st.setByte(i, x))
  def ~?(x: Date): This = ~?((st: PreparedStatement, i: Int) => st.setDate(i, x))
  def ~?(x: Double): This = ~?((st: PreparedStatement, i: Int) => st.setDouble(i, x))
  def ~?(x: Float): This = ~?((st: PreparedStatement, i: Int) => st.setFloat(i, x))
  def ~?(x: Int): This = ~?((st: PreparedStatement, i: Int) => st.setInt(i, x))
  def ~?(x: Long): This = ~?((st: PreparedStatement, i: Int) => st.setLong(i, x))
  def ~?(x: Short): This = ~?((st: PreparedStatement, i: Int) => st.setShort(i, x))
  def ~?(x: String): This = ~?((st: PreparedStatement, i: Int) => st.setString(i, x))
  def ~?(x: Time): This = ~?((st: PreparedStatement, i: Int) => st.setTime(i, x))
  def ~?(x: Timestamp): This = ~?((st: PreparedStatement, i: Int) => st.setTimestamp(i, x))

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
    var i = varSettersLen
    varSetters foreach { x =>
      x(st, i)
      i -= 1
    }
  }

  protected def getStatement = query
}


class DynamicQuery[+T](implicit rconv: PositionedResult => T) extends DynamicQueryBase[T,DynamicQuery[T]] {
  def select = this ~ "select"
  def select(s: String) = this ~ "select" ~ s
  protected def extractValue(rs: PositionedResult): T = rconv(rs)
}

class DynamicUpdate extends DynamicQueryBase[Int,DynamicUpdate] {
  def insert = this ~ "insert"
  def insert(s: String) = this ~ "insert" ~ s
  def update = this ~ "update"
  def update(s: String) = this ~ "update" ~ s
  protected def extractValue(rs: PositionedResult): Int =
    throw new SQueryException("DynamicUpdate.extractValue called; Non-query statements should not return a ResultSet")
}
