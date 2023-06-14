package slick.jdbc

import java.sql.{PreparedStatement, ResultSet}

import slick.relational.*
import slick.SlickException


/** Specialized JDBC ResultConverter for non-`Option` values. */
class BaseResultConverter[T](val jdbcType: JdbcType[T], val columnName: String, val index: Int)
  extends ResultConverter[JdbcResultConverterDomain, T] {
  def read(pr: ResultSet) = {
    val v = jdbcType.getValue(pr, index)
    if (jdbcType.wasNull(pr, index)) throw new SlickException("Read NULL value for ResultSet column " + columnName)
    v
  }
  def update(value: T, pr: ResultSet) = jdbcType.updateValue(value, pr, index)
  def set(value: T, pp: PreparedStatement, offset: Int) =
    jdbcType.setValue(value, pp, index + offset)
  override def getDumpInfo =
    super.getDumpInfo.copy(mainInfo = s"idx=$index, name=$columnName", attrInfo = ": " + jdbcType)
  def width = 1
}

/**
 * Specialized JDBC ResultConverter for handling values of type `Option[T]`.
 * Boxing is avoided when the result is `None`. */
class OptionResultConverter[T](val jdbcType: JdbcType[T], val index: Int)
  extends ResultConverter[JdbcResultConverterDomain, Option[T]] {
  def read(pr: ResultSet) = {
    val v = jdbcType.getValue(pr, index)
    if(jdbcType.wasNull(pr, index)) None else Some(v)
  }
  def update(value: Option[T], pr: ResultSet) = value match {
    case Some(v) => jdbcType.updateValue(v, pr, index)
    case _ => jdbcType.updateNull(pr, index)
  }
  def set(value: Option[T], pp: PreparedStatement, offset: Int) = value match {
    case Some(v) => jdbcType.setValue(v, pp, index + offset)
    case _ => jdbcType.setNull(pp, index + offset)
  }
  override def getDumpInfo = super.getDumpInfo.copy(mainInfo = s"idx=$index", attrInfo = ": " + jdbcType)
  def width = 1
  def getOrElse(default: () => T): DefaultingResultConverter[T] =
    if(jdbcType.scalaType.isPrimitive) new DefaultingResultConverter[T](jdbcType, default, index)
    else new DefaultingResultConverter[T](jdbcType, default, index) {
      override def read(pr: ResultSet) = {
        val v = ti.getValue(pr, this.index)
        if(v.asInstanceOf[AnyRef] eq null) computeDefault() else v
      }
    }
  def isDefined = new IsDefinedResultConverter[T](jdbcType, index)
}

/** Specialized JDBC ResultConverter for handling non-`Option` values with a default.
 * A (possibly specialized) function for the default value is used to translate SQL `NULL` values. */
class DefaultingResultConverter[T](val ti: JdbcType[T], val computeDefault: () => T, val index: Int)
  extends ResultConverter[JdbcResultConverterDomain, T] {

  def read(pr: ResultSet) = {
    val v = ti.getValue(pr, index)
    if (ti.wasNull(pr, index)) computeDefault() else v
  }
  def update(value: T, pr: ResultSet) = ti.updateValue(value, pr, index)
  def set(value: T, pp: PreparedStatement, offset: Int) = ti.setValue(value, pp, index + offset)
  override def getDumpInfo =
    super.getDumpInfo.copy(
      mainInfo =
        s"idx=$index, default=" + {
          try computeDefault() catch {
            case e: Throwable => "[" + e.getClass.getName + "]"
          }
        },
      attrInfo = ": " + ti
    )
  def width = 1
}

/** Specialized JDBC ResultConverter for handling `isDefined` checks for `Option` values. */
class IsDefinedResultConverter[
  T](val ti: JdbcType[T], val idx: Int) extends ResultConverter[JdbcResultConverterDomain, Boolean] {
  def read(pr: ResultSet) = {
    ti.getValue(pr, idx)
    !ti.wasNull(pr, idx)
  }
  override def update(value: Boolean, pr: ResultSet): Nothing =
    throw new SlickException("Cannot insert/update IsDefined check")
  override def set(value: Boolean, pp: PreparedStatement, offset: Int): Nothing =
    throw new SlickException("Cannot insert/update IsDefined check")
  def width = 1
  override def getDumpInfo = super.getDumpInfo.copy(mainInfo = s"idx=$idx", attrInfo = ": " + ti)
}
