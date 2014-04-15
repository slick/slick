package scala.slick.jdbc

import scala.language.existentials
import java.sql.{PreparedStatement, ResultSet}
import scala.slick.relational._
import scala.slick.SlickException
import scala.slick.ast.{ScalaBaseType, Dump}

/** Specialized JDBC ResultConverter for non-`Option` values. */
class BaseResultConverter[@specialized(Byte, Short, Int, Long, Char, Float, Double, Boolean) T](val ti: JdbcType[T], val name: String, val fullIdx: Int, val skippingIdx: Int) extends ResultConverter[JdbcResultConverterDomain, T] {
  def read(pr: ResultSet) = {
    val v = ti.getValue(pr, fullIdx)
    if(ti.wasNull(pr, fullIdx)) throw new SlickException("Read NULL value for ResultSet column "+name)
    v
  }
  def update(value: T, pr: ResultSet) = ti.updateValue(value, pr, fullIdx)
  def set(value: T, pp: PreparedStatement, forced: Boolean) =
    if(forced) ti.setValue(value, pp, fullIdx)
    else if(skippingIdx > 0) ti.setValue(value, pp, skippingIdx)
  override def info = super.info + "(" + Dump.blue + ti + Dump.normal + s", fullIdx=$fullIdx, skippingIdx=$skippingIdx, name=$name)"
  def fullWidth = 1
  def skippingWidth = if(skippingIdx == 0) 0 else 1
}

/** Specialized JDBC ResultConverter for handling values of type `Option[T]`.
  * Boxing is avoided when the result is `None`. */
class OptionResultConverter[@specialized(Byte, Short, Int, Long, Char, Float, Double, Boolean) T](val ti: JdbcType[T], val fullIdx: Int, val skippingIdx: Int) extends ResultConverter[JdbcResultConverterDomain, Option[T]] {
  def read(pr: ResultSet) = {
    val v = ti.getValue(pr, fullIdx)
    if(ti.wasNull(pr, fullIdx)) None else Some(v)
  }
  def update(value: Option[T], pr: ResultSet) = value match {
    case Some(v) => ti.updateValue(v, pr, fullIdx)
    case _ => ti.updateNull(pr, fullIdx)
  }
  def set(value: Option[T], pp: PreparedStatement, forced: Boolean) = value match {
    case Some(v) => if(forced) ti.setValue(v, pp, fullIdx) else if(skippingIdx > 0) ti.setValue(v, pp, skippingIdx)
    case _ => if(forced) ti.setNull(pp, fullIdx) else if(skippingIdx > 0) ti.setNull(pp, skippingIdx)
  }
  override def info = super.info + "(" + Dump.blue + ti + Dump.normal + s", fullIdx=$fullIdx, skippingIdx=$skippingIdx)"
  def fullWidth = 1
  def skippingWidth = if(skippingIdx == 0) 0 else 1
  def getOrElse(default: () => T): DefaultingResultConverter[T] =
    if(ti.scalaType.isPrimitive) new DefaultingResultConverter[T](ti, default, fullIdx, skippingIdx)
    else new DefaultingResultConverter[T](ti, default, fullIdx, skippingIdx) {
        override def read(pr: ResultSet) = {
          val v = ti.getValue(pr, fullIdx)
          if(v.asInstanceOf[AnyRef] eq null) default() else v
        }
      }
}

/** Specialized JDBC ResultConverter for handling non-`Option`values with a default.
  * A (possibly specialized) function for the default value is used to translate SQL `NULL` values. */
class DefaultingResultConverter[@specialized(Byte, Short, Int, Long, Char, Float, Double, Boolean) T](val ti: JdbcType[T], val default: () => T, val fullIdx: Int, val skippingIdx: Int) extends ResultConverter[JdbcResultConverterDomain, T] {
  def read(pr: ResultSet) = {
    val v = ti.getValue(pr, fullIdx)
    if(ti.wasNull(pr, fullIdx)) default() else v
  }
  def update(value: T, pr: ResultSet) = ti.updateValue(value, pr, fullIdx)
  def set(value: T, pp: PreparedStatement, forced: Boolean) =
    if(forced) ti.setValue(value, pp, fullIdx)
    else if(skippingIdx > 0) ti.setValue(value, pp, skippingIdx)
  override def info =
    super.info + "(" + Dump.blue + ti + Dump.normal + ", fullIdx=" + fullIdx + ", skippingIdx=" + skippingIdx + ", default=" +
      { try default() catch { case e: Throwable => "["+e.getClass.getName+"]" } } + ")"
  def fullWidth = 1
  def skippingWidth = if(skippingIdx == 0) 0 else 1
}

/** A `ResultConverter` that simplifies the implementation of fast path
  * converters for `JdbcProfile`. It always wraps a `TypeMappingResultConverter`
  * on top of a `ProductResultConverter`, allowing direct access to the product
  * elements. */
abstract class JdbcFastPath[T](protected[this] val rc: TypeMappingResultConverter[JdbcResultConverterDomain, T, _]) extends ResultConverter[JdbcResultConverterDomain, T] {
  private[this] val ch = rc.child.asInstanceOf[ProductResultConverter[JdbcResultConverterDomain, _]].elementConverters
  private[this] var idx = -1

  /** Return the next specialized child `ResultConverter` for the specified type. */
  protected[this] def next[C] = {
    idx += 1
    ch(idx).asInstanceOf[ResultConverter[JdbcResultConverterDomain, C]]
  }

  def read(pr: Reader) = rc.read(pr)
  def update(value: T, pr: Updater) = rc.update(value, pr)
  def set(value: T, pp: Writer, forced: Boolean) = rc.set(value, pp, forced)

  override def children = Iterator(rc)
  override def info = super.info + Dump.yellow + " [FastPath]" + Dump.normal
  def fullWidth = rc.fullWidth
  def skippingWidth = rc.skippingWidth
}
