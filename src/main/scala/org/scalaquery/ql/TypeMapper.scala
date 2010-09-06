package org.scalaquery.ql

import java.sql.{Blob, Clob, Date, Time, Timestamp}
import org.scalaquery.SQueryException
import org.scalaquery.ql.basic.BasicProfile
import org.scalaquery.session.{PositionedParameters, PositionedResult}

sealed trait TypeMapper[T] extends (BasicProfile => TypeMapperDelegate[T]) { self =>
  def createOptionTypeMapper: OptionTypeMapper[T] = new OptionTypeMapper[T](self) {
    def apply(profile: BasicProfile) = self(profile).createOptionTypeMapperDelegate
  }
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

trait BaseTypeMapper[T] extends TypeMapper[T]

abstract class OptionTypeMapper[T](val base: TypeMapper[T]) extends TypeMapper[Option[T]]

trait NumericTypeMapper

trait TypeMapperDelegate[T] { self =>
  def zero: T
  def sqlType: Int
  def sqlTypeName: String = TypeMapperDelegate.typeNames.getOrElse(sqlType,
    throw new SQueryException("No SQL type name found in java.sql.Types for code "+sqlType))
  def setValue(v: T, p: PositionedParameters): Unit
  def setOption(v: Option[T], p: PositionedParameters): Unit
  def nextValue(r: PositionedResult): T
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
