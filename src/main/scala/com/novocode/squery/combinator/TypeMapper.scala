package com.novocode.squery.combinator

import java.sql.{Blob, Clob, Date, Time, Timestamp}
import com.novocode.squery.combinator.basic.BasicProfile
import com.novocode.squery.session.{PositionedParameters, PositionedResult}

sealed trait TypeMapper[B,V] extends (BasicProfile => TypeMapperDelegate[V]) {
  final type Base = B
  final type Value = V
  type OM <: NewOptionMapper
  val om: OM
  def createOptionTypeMapper: OptionTypeMapper[V] = new OptionTypeMapper[V](this)
}

object TypeMapper {
  implicit def typeMapperToOptionTypeMapper[T](implicit t: TypeMapper[T,T]): OptionTypeMapper[T] = t.createOptionTypeMapper

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

trait BaseTypeMapper[T] extends TypeMapper[T,T] {
  final type OM = NewOptionMapper.False.type
  final val om = NewOptionMapper.False
}

final class OptionTypeMapper[T](val base: TypeMapper[_,T]) extends TypeMapper[T,Option[T]] {
  type OM = NewOptionMapper.True.type
  val om = NewOptionMapper.True
  def apply(profile: BasicProfile) = base(profile).createOptionTypeMapperDelegate
}

trait NumericTypeMapper

trait TypeMapperDelegate[T] { self =>
  def zero: T
  def sqlType: Int
  def setValue(v: T, p: PositionedParameters): Unit
  def setOption(v: Option[T], p: PositionedParameters): Unit
  def nextValue(r: PositionedResult): T
  final def nextValueOrElse(d: =>T, r: PositionedResult) = { val v = nextValue(r); if(r.rs wasNull) d else v }
  final def nextOption(r: PositionedResult): Option[T] = { val v = nextValue(r); if(r.rs wasNull) None else Some(v) }
  def valueToSQLLiteral(value: T): String = value.toString

  def createOptionTypeMapperDelegate: TypeMapperDelegate[Option[T]] = new TypeMapperDelegate[Option[T]] {
    def zero = None
    def sqlType = self.sqlType
    def setValue(v: Option[T], p: PositionedParameters) = self.setOption(v, p)
    def setOption(v: Option[Option[T]], p: PositionedParameters) = self.setOption(v.getOrElse(None), p)
    def nextValue(r: PositionedResult) = self.nextOption(r)
    override def valueToSQLLiteral(value: Option[T]): String = value.map(self.valueToSQLLiteral).getOrElse("null")
  }
}

trait NewOptionMapper {
  type Plus[Other <: NewOptionMapper]
  type Apply[T]
  def apply[B](c: Column[B]): Column[Apply[B]]
  def +[O <: NewOptionMapper](o: O): Plus[O]
}

object NewOptionMapper {
  object True extends NewOptionMapper {
    type Plus[Other <: NewOptionMapper] = True.type
    type Apply[T] = Option[T]
    def apply[B](c: Column[B]) = c.?
    def +[O <: NewOptionMapper](o: O): Plus[O] = this
  }

  object False extends NewOptionMapper {
    type Plus[Other <: NewOptionMapper] = Other
    type Apply[T] = T
    def apply[B](c: Column[B]) = c
    def +[O <: NewOptionMapper](o: O): Plus[O] = o
  }
}
