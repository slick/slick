package slick.jdbc

import java.sql.{PreparedStatement, ResultSet}
import slick.relational._
import slick.SlickException

/** Specialized JDBC ResultConverter for non-`Option` values. */
class BaseResultConverter[@specialized(Byte, Short, Int, Long, Char, Float, Double, Boolean) T](val ti: JdbcType[T], val name: String, val idx: Int) extends ResultConverter[JdbcResultConverterDomain, T] {
  def read(pr: ResultSet) = {
    val v = ti.getValue(pr, idx)
    if(ti.wasNull(pr, idx)) throw new SlickException("Read NULL value for ResultSet column "+name)
    v
  }
  def update(value: T, pr: ResultSet) = ti.updateValue(value, pr, idx)
  def set(value: T, pp: PreparedStatement) =
    ti.setValue(value, pp, idx)
  override def getDumpInfo = super.getDumpInfo.copy(mainInfo = s"idx=$idx, name=$name", attrInfo = ": " + ti)
  def width = 1
}

/** Specialized JDBC ResultConverter for handling values of type `Option[T]`.
  * Boxing is avoided when the result is `None`. */
class OptionResultConverter[@specialized(Byte, Short, Int, Long, Char, Float, Double, Boolean) T](val ti: JdbcType[T], val idx: Int) extends ResultConverter[JdbcResultConverterDomain, Option[T]] {
  def read(pr: ResultSet) = {
    val v = ti.getValue(pr, idx)
    if(ti.wasNull(pr, idx)) None else Some(v)
  }
  def update(value: Option[T], pr: ResultSet) = value match {
    case Some(v) => ti.updateValue(v, pr, idx)
    case _ => ti.updateNull(pr, idx)
  }
  def set(value: Option[T], pp: PreparedStatement) = value match {
    case Some(v) => ti.setValue(v, pp, idx)
    case _ => ti.setNull(pp, idx)
  }
  override def getDumpInfo = super.getDumpInfo.copy(mainInfo = s"idx=$idx", attrInfo = ": " + ti)
  def width = 1
  def getOrElse(default: () => T): DefaultingResultConverter[T] =
    if(ti.scalaType.isPrimitive) new DefaultingResultConverter[T](ti, default, idx)
    else new DefaultingResultConverter[T](ti, default, idx) {
      override def read(pr: ResultSet) = {
        val v = ti.getValue(pr, idx)
        if(v.asInstanceOf[AnyRef] eq null) default() else v
      }
    }
  def isDefined = new IsDefinedResultConverter[T](ti, idx)
}

/** Specialized JDBC ResultConverter for handling non-`Option` values with a default.
  * A (possibly specialized) function for the default value is used to translate SQL `NULL` values. */
class DefaultingResultConverter[@specialized(Byte, Short, Int, Long, Char, Float, Double, Boolean) T](val ti: JdbcType[T], val default: () => T, val idx: Int) extends ResultConverter[JdbcResultConverterDomain, T] {
  def read(pr: ResultSet) = {
    val v = ti.getValue(pr, idx)
    if(ti.wasNull(pr, idx)) default() else v
  }
  def update(value: T, pr: ResultSet) = ti.updateValue(value, pr, idx)
  def set(value: T, pp: PreparedStatement) = ti.setValue(value, pp, idx)
  override def getDumpInfo = super.getDumpInfo.copy(mainInfo = s"idx=$idx, default=" +
    { try default() catch { case e: Throwable => "["+e.getClass.getName+"]" } },
    attrInfo = ": " + ti)
  def width = 1
}

/** Specialized JDBC ResultConverter for handling `isDefined` checks for `Option` values. */
class IsDefinedResultConverter[@specialized(Byte, Short, Int, Long, Char, Float, Double, Boolean) T](val ti: JdbcType[T], val idx: Int) extends ResultConverter[JdbcResultConverterDomain, Boolean] {
  def read(pr: ResultSet) = {
    ti.getValue(pr, idx)
    !ti.wasNull(pr, idx)
  }
  def update(value: Boolean, pr: ResultSet) =
    throw new SlickException("Cannot insert/update IsDefined check")
  def set(value: Boolean, pp: PreparedStatement) =
    throw new SlickException("Cannot insert/update IsDefined check")
  def width = 1
  override def getDumpInfo = super.getDumpInfo.copy(mainInfo = s"idx=$idx", attrInfo = ": " + ti)
}
