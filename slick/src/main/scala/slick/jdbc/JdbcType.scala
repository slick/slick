package slick.jdbc

import java.sql.{PreparedStatement, ResultSet}

import slick.ast.{BaseTypedType, FieldSymbol, NumericTypedType}
import java.sql.{Blob, Clob, Date, Time, Timestamp}
import java.time.{Instant, LocalDate, LocalDateTime, LocalTime, OffsetDateTime, OffsetTime, ZonedDateTime}
import java.util.UUID

/** A JdbcType object represents a Scala type that can be used as a column type in the database.
  * Implicit JdbcTypes for the standard types are provided by the profile. */
trait JdbcType[@specialized(Byte, Short, Int, Long, Char, Float, Double, Boolean) T] extends BaseTypedType[T] { self =>
  /** The constant from java.sql.Types that is used for setting parameters of the type to NULL. */
  def sqlType: Int
  /** The default name for the SQL type that is used for column declarations. */
  def sqlTypeName(size: Option[FieldSymbol]): String
  /** Set a parameter of the type. */
  def setValue(v: T, p: PreparedStatement, idx: Int): Unit
  /** Set a parameter of the type to NULL. */
  def setNull(p: PreparedStatement, idx: Int): Unit
  /** Set an Option parameter of the type. */
  final def setOption(v: Option[T], p: PreparedStatement, idx: Int): Unit = v match {
    case Some(v) => setValue(v, p, idx)
    case None => setNull(p, idx)
  }

  /** Get a result column of the type. For reference types, SQL NULL values
    * are returned as `null`, for primitive types a default value is returned. */
  def getValue(r: ResultSet, idx: Int): T
  /** Check if the value returned by the immediately preceding call to
    * getValue() was NULL. */
  def wasNull(r: ResultSet, idx: Int): Boolean

  /** Update a column of the type in a mutable result set. */
  def updateValue(v: T, r: ResultSet, idx: Int): Unit
  /** Update a column of the type in a mutable result set with NULL. */
  def updateNull(r: ResultSet, idx: Int): Unit = r.updateNull(idx)

  /** Convert a value to a SQL literal.
    * This should throw a `SlickException` if `hasLiteralForm` is false. */
  def valueToSQLLiteral(value: T): String

  /** Indicates whether values of this type have a literal representation in
    * SQL statements.
    * This must return false if `valueToSQLLiteral` throws a SlickException.
    * QueryBuilder (and profile-specific subclasses thereof) uses this method
    * to treat LiteralNodes as volatile (i.e. using bind variables) as needed. */
  def hasLiteralForm: Boolean

  override def toString = scalaType.toString + "'"
}
object JdbcType {
  object Primitives {
    // Note implicits for String, BigDecimal and all primitives are already imported when profile.api._ is imported
    // TODO decide on whether to remove this or not...
    implicit def booleanJdbcType(implicit types: ApiJdbcTypes): JdbcType[Boolean] = types.booleanJdbcType
    implicit def bigDecimalJdbcType(implicit types: ApiJdbcTypes): JdbcType[BigDecimal] with NumericTypedType = types.bigDecimalJdbcType
    implicit def byteJdbcType(implicit types: ApiJdbcTypes): JdbcType[Byte] with NumericTypedType = types.byteJdbcType
    implicit def charJdbcType(implicit types: ApiJdbcTypes): JdbcType[Char] = types.charJdbcType
    implicit def doubleJdbcType(implicit types: ApiJdbcTypes): JdbcType[Double] with NumericTypedType = types.doubleJdbcType
    implicit def floatJdbcType(implicit types: ApiJdbcTypes): JdbcType[Float] with NumericTypedType = types.floatJdbcType
    implicit def intJdbcType(implicit types: ApiJdbcTypes): JdbcType[Int] with NumericTypedType = types.intJdbcType
    implicit def longJdbcType(implicit types: ApiJdbcTypes): JdbcType[Long] with NumericTypedType = types.longJdbcType
    implicit def shortJdbcType(implicit types: ApiJdbcTypes): JdbcType[Short] with NumericTypedType = types.shortJdbcType
    implicit def stringJdbcType(implicit types: ApiJdbcTypes): JdbcType[String] = types.stringJdbcType
  }

  implicit def blobColumnType(implicit types: ApiJdbcTypes): JdbcType[Blob] = types.blobJdbcType
  implicit def byteArrayColumnType(implicit types: ApiJdbcTypes): JdbcType[Array[Byte]] = types.byteArrayJdbcType
  implicit def clobColumnType(implicit types: ApiJdbcTypes): JdbcType[Clob] = types.clobJdbcType
  implicit def dateColumnType(implicit types: ApiJdbcTypes): JdbcType[Date] = types.dateJdbcType
  implicit def offsetDateTimeColumnType(implicit types: ApiJdbcTypes): JdbcType[OffsetDateTime] = types.offsetDateTimeType
  implicit def zonedDateColumnType(implicit types: ApiJdbcTypes): JdbcType[ZonedDateTime] = types.zonedDateType
  implicit def localTimeColumnType(implicit types: ApiJdbcTypes): JdbcType[LocalTime] = types.localTimeType
  implicit def localDateColumnType(implicit types: ApiJdbcTypes): JdbcType[LocalDate] = types.localDateType
  implicit def localDateTimeColumnType(implicit types: ApiJdbcTypes): JdbcType[LocalDateTime] = types.localDateTimeType
  implicit def offsetTimeColumnType(implicit types: ApiJdbcTypes): JdbcType[OffsetTime] = types.offsetTimeType
  implicit def instantColumnType(implicit types: ApiJdbcTypes): JdbcType[Instant] = types.instantType
  implicit def timeColumnType(implicit types: ApiJdbcTypes): JdbcType[Time] = types.timeJdbcType
  implicit def timestampColumnType(implicit types: ApiJdbcTypes): JdbcType[Timestamp] = types.timestampJdbcType
  implicit def uuidColumnType(implicit types: ApiJdbcTypes): JdbcType[UUID] = types.uuidJdbcType
  implicit def nullColumnType(implicit types: ApiJdbcTypes): JdbcType[Null] = types.nullJdbcType
}
