package slick.jdbc

import java.sql.{Blob, Clob, Date, Time, Timestamp}
import java.time.{Instant, LocalDate, LocalDateTime, LocalTime, OffsetDateTime, OffsetTime, ZonedDateTime}
import java.util.UUID

import slick.ast.{BaseTypedType, NumericTypedType, ScalaBaseType, ScalaNumericType, TypedType}

import scala.language.higherKinds

trait ApiJdbcTypes {
  def booleanJdbcType: JdbcType[Boolean]
  def bigDecimalJdbcType: JdbcType[BigDecimal] with NumericTypedType
  def byteJdbcType: JdbcType[Byte] with NumericTypedType
  def charJdbcType: JdbcType[Char]
  def doubleJdbcType: JdbcType[Double] with NumericTypedType
  def floatJdbcType: JdbcType[Float] with NumericTypedType
  def intJdbcType: JdbcType[Int] with NumericTypedType
  def longJdbcType: JdbcType[Long] with NumericTypedType
  def shortJdbcType: JdbcType[Short] with NumericTypedType
  def stringJdbcType: JdbcType[String]
  def blobJdbcType: JdbcType[Blob]
  def byteArrayJdbcType: JdbcType[Array[Byte]]
  def clobJdbcType: JdbcType[Clob]
  def dateJdbcType: JdbcType[Date]
  def offsetDateTimeType: JdbcType[OffsetDateTime]
  def zonedDateType: JdbcType[ZonedDateTime]
  def localTimeType: JdbcType[LocalTime]
  def localDateType: JdbcType[LocalDate]
  def localDateTimeType: JdbcType[LocalDateTime]
  def offsetTimeType: JdbcType[OffsetTime]
  def instantType: JdbcType[Instant]
  def timeJdbcType: JdbcType[Time]
  def timestampJdbcType: JdbcType[Timestamp]
  def uuidJdbcType: JdbcType[UUID]
  def nullJdbcType: JdbcType[Null]
}
