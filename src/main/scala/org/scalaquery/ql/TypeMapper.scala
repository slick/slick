package org.scalaquery.ql

import java.sql.{Blob, Clob, Date, Time, Timestamp}
import org.scalaquery.SQueryException
import org.scalaquery.ql.basic.BasicProfile
import org.scalaquery.session.{PositionedParameters, PositionedResult}

/**
 * A (usually implicit) TypeMapper object represents a Scala type that can be
 * used as a column type in the database. The actual implementation of the
 * type is deferred to a TypeMapperDelegate which can depend on the driver.
 * 
 * <p>Custom types with a single implementation can implement both traits in
 * one object:</p>
 * <code><pre>
 * implicit object MyTypeMapper
 *     extends TypeMapper[MyType] with TypeMapperDelegate[MyType] {
 *   def apply(p: BasicProfile) = this
 *   def zero = ...
 *   def sqlType = ...
 *   def setValue(v: Long, p: PositionedParameters) = ...
 *   def setOption(v: Option[Long], p: PositionedParameters) = ...
 *   def nextValue(r: PositionedResult) = ...
 *   def updateValue(v: Long, r: PositionedResult) = ...
 * }
 * </pre></code>
 */
sealed trait TypeMapper[T] extends (BasicProfile => TypeMapperDelegate[T]) { self =>
  def createOptionTypeMapper: OptionTypeMapper[T] = new OptionTypeMapper[T](self) {
    def apply(profile: BasicProfile) = self(profile).createOptionTypeMapperDelegate
    def getBaseTypeMapper[U](implicit ev: Option[U] =:= Option[T]): TypeMapper[U] = self.asInstanceOf[TypeMapper[U]]
  }
  def getBaseTypeMapper[U](implicit ev: Option[U] =:= T): TypeMapper[U]
}

object TypeMapper {
  implicit def typeMapperToOptionTypeMapper[T](implicit t: TypeMapper[T]): OptionTypeMapper[T] = t.createOptionTypeMapper

  implicit object BooleanTypeMapper extends BaseTypeMapper[Boolean] {
    def apply(profile: BasicProfile) = profile.typeMapperDelegates.booleanTypeMapperDelegate
  }

  implicit object BlobTypeMapper extends BaseTypeMapper[Blob] {
    def apply(profile: BasicProfile) = profile.typeMapperDelegates.blobTypeMapperDelegate
  }

  implicit object ByteTypeMapper extends BaseTypeMapper[Byte] with NumericTypeMapper {
    def apply(profile: BasicProfile) = profile.typeMapperDelegates.byteTypeMapperDelegate
  }

  implicit object ByteArrayTypeMapper extends BaseTypeMapper[Array[Byte]] {
    def apply(profile: BasicProfile) = profile.typeMapperDelegates.byteArrayTypeMapperDelegate
  }

  implicit object ClobTypeMapper extends BaseTypeMapper[Clob] {
    def apply(profile: BasicProfile) = profile.typeMapperDelegates.clobTypeMapperDelegate
  }

  implicit object DateTypeMapper extends BaseTypeMapper[Date] {
    def apply(profile: BasicProfile) = profile.typeMapperDelegates.dateTypeMapperDelegate
  }

  implicit object DoubleTypeMapper extends BaseTypeMapper[Double] with NumericTypeMapper {
    def apply(profile: BasicProfile) = profile.typeMapperDelegates.doubleTypeMapperDelegate
  }

  implicit object FloatTypeMapper extends BaseTypeMapper[Float] with NumericTypeMapper {
    def apply(profile: BasicProfile) = profile.typeMapperDelegates.floatTypeMapperDelegate
  }

  implicit object IntTypeMapper extends BaseTypeMapper[Int] with NumericTypeMapper {
    def apply(profile: BasicProfile) = profile.typeMapperDelegates.intTypeMapperDelegate
  }

  implicit object LongTypeMapper extends BaseTypeMapper[Long] with NumericTypeMapper {
    def apply(profile: BasicProfile) = profile.typeMapperDelegates.longTypeMapperDelegate
  }

  implicit object StringTypeMapper extends BaseTypeMapper[String] {
    def apply(profile: BasicProfile) = profile.typeMapperDelegates.stringTypeMapperDelegate
  }

  implicit object TimeTypeMapper extends BaseTypeMapper[Time] {
    def apply(profile: BasicProfile) = profile.typeMapperDelegates.timeTypeMapperDelegate
  }

  implicit object TimestampTypeMapper extends BaseTypeMapper[Timestamp] {
    def apply(profile: BasicProfile) = profile.typeMapperDelegates.timestampTypeMapperDelegate
  }

  object NullTypeMapper extends BaseTypeMapper[Null] {
    def apply(profile: BasicProfile) = profile.typeMapperDelegates.nullTypeMapperDelegate
  }
}

trait BaseTypeMapper[T] extends TypeMapper[T] {
  def getBaseTypeMapper[U](implicit ev: Option[U] =:= T) =
    throw new SQueryException("A BaseTypeMapper should not have an Option type")
}

abstract class OptionTypeMapper[T](val base: TypeMapper[T]) extends TypeMapper[Option[T]]

/**
 * Adding this marker trait to a TypeMapper makes the type eligible for
 * numeric operators.
 */
trait NumericTypeMapper

trait TypeMapperDelegate[T] { self =>
  /**
   * A zero value for the type. This is used as a default instead of NULL when
   * used as a non-nullable column.
   */
  def zero: T
  /**
   * The constant from java.sql.Types that is used for setting parameters of
   * the type to NULL.
   */
  def sqlType: Int
  /**
   * The default name for the SQL type that is used for column declarations.
   */
  def sqlTypeName: String = TypeMapperDelegate.typeNames.getOrElse(sqlType,
    throw new SQueryException("No SQL type name found in java.sql.Types for code "+sqlType))
  /**
   * Set a parameter of the type.
   */
  def setValue(v: T, p: PositionedParameters): Unit
  /**
   * Set an Option parameter of the type.
   */
  def setOption(v: Option[T], p: PositionedParameters): Unit
  /**
   * Get a result column of the type.
   */
  def nextValue(r: PositionedResult): T
  /**
   * Update a column of the type in a mutable result set.
   */
  def updateValue(v: T, r: PositionedResult): Unit
  final def nextValueOrElse(d: =>T, r: PositionedResult) = { val v = nextValue(r); if(r.rs wasNull) d else v }
  final def nextOption(r: PositionedResult): Option[T] = { val v = nextValue(r); if(r.rs wasNull) None else Some(v) }
  final def updateOption(v: Option[T], r: PositionedResult): Unit = v match {
    case Some(s) => updateValue(s, r)
    case None => r.updateNull()
  }
  def valueToSQLLiteral(value: T): String = value.toString
  def nullable = false

  def createOptionTypeMapperDelegate: TypeMapperDelegate[Option[T]] = new TypeMapperDelegate[Option[T]] {
    def zero = None
    def sqlType = self.sqlType
    override def sqlTypeName = self.sqlTypeName
    def setValue(v: Option[T], p: PositionedParameters) = self.setOption(v, p)
    def setOption(v: Option[Option[T]], p: PositionedParameters) = self.setOption(v.getOrElse(None), p)
    def nextValue(r: PositionedResult) = self.nextOption(r)
    def updateValue(v: Option[T], r: PositionedResult) = self.updateOption(v, r)
    override def valueToSQLLiteral(value: Option[T]): String = value.map(self.valueToSQLLiteral).getOrElse("null")
    override def nullable = true
  }
}

object TypeMapperDelegate {
  private[scalaquery] lazy val typeNames = Map() ++
  (for(f <- classOf[java.sql.Types].getFields)
    yield f.get(null).asInstanceOf[Int] -> f.getName)
}
