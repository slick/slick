package com.novocode.squery.session

import java.sql.{Blob, Clob, Date, Time, Timestamp}

sealed trait TypeMapper[T] {
  def zero: T
  def sqlType: Int
  def setValue(v: T, p: PositionedParameters): Unit
  def setOption(v: Option[T], p: PositionedParameters): Unit
  def nextValue(r: PositionedResult): T
  final def nextValueOrElse(d: =>T, r: PositionedResult) = { val v = nextValue(r); if(r.rs wasNull) d else v }
  final def nextOption(r: PositionedResult): Option[T] = { val v = nextValue(r); if(r.rs wasNull) None else Some(v) }
  def valueToSQLLiteral(value: T): String = value.toString
}

trait BaseTypeMapper[T] extends TypeMapper[T]

trait OptionTypeMapper[T] extends TypeMapper[Option[T]]

trait NumericType

object TypeMapper {
  implicit object BooleanTypeMapper extends BaseTypeMapper[Boolean] {
    def zero = false
    def sqlType = java.sql.Types.BOOLEAN
    def setValue(v: Boolean, p: PositionedParameters) = p.setBoolean(v)
    def setOption(v: Option[Boolean], p: PositionedParameters) = p.setBooleanOption(v)
    def nextValue(r: PositionedResult) = r.nextBoolean
  }

  implicit object BlobTypeMapper extends BaseTypeMapper[Blob] {
    def zero = null
    def sqlType = java.sql.Types.BLOB
    def setValue(v: Blob, p: PositionedParameters) = p.setBlob(v)
    def setOption(v: Option[Blob], p: PositionedParameters) = p.setBlobOption(v)
    def nextValue(r: PositionedResult) = r.nextBlob
  }

  implicit object ByteTypeMapper extends BaseTypeMapper[Byte] with NumericType {
    def zero = 0
    def sqlType = java.sql.Types.TINYINT
    def setValue(v: Byte, p: PositionedParameters) = p.setByte(v)
    def setOption(v: Option[Byte], p: PositionedParameters) = p.setByteOption(v)
    def nextValue(r: PositionedResult) = r.nextByte
  }

  implicit object ClobTypeMapper extends BaseTypeMapper[Clob] {
    def zero = null
    def sqlType = java.sql.Types.CLOB
    def setValue(v: Clob, p: PositionedParameters) = p.setClob(v)
    def setOption(v: Option[Clob], p: PositionedParameters) = p.setClobOption(v)
    def nextValue(r: PositionedResult) = r.nextClob
  }

  implicit object DateTypeMapper extends BaseTypeMapper[Date] {
    def zero = new Date(70, 0, 1)
    def sqlType = java.sql.Types.DATE
    def setValue(v: Date, p: PositionedParameters) = p.setDate(v)
    def setOption(v: Option[Date], p: PositionedParameters) = p.setDateOption(v)
    def nextValue(r: PositionedResult) = r.nextDate
  }

  implicit object DoubleTypeMapper extends BaseTypeMapper[Double] with NumericType {
    def zero = 0
    def sqlType = java.sql.Types.DOUBLE
    def setValue(v: Double, p: PositionedParameters) = p.setDouble(v)
    def setOption(v: Option[Double], p: PositionedParameters) = p.setDoubleOption(v)
    def nextValue(r: PositionedResult) = r.nextDouble
  }

  implicit object FloatTypeMapper extends BaseTypeMapper[Float] with NumericType {
    def zero = 0
    def sqlType = java.sql.Types.FLOAT
    def setValue(v: Float, p: PositionedParameters) = p.setFloat(v)
    def setOption(v: Option[Float], p: PositionedParameters) = p.setFloatOption(v)
    def nextValue(r: PositionedResult) = r.nextFloat
  }

  implicit object IntTypeMapper extends BaseTypeMapper[Int] with NumericType {
    def zero = 0
    def sqlType = java.sql.Types.INTEGER
    def setValue(v: Int, p: PositionedParameters) = p.setInt(v)
    def setOption(v: Option[Int], p: PositionedParameters) = p.setIntOption(v)
    def nextValue(r: PositionedResult) = r.nextInt
  }

  implicit object LongTypeMapper extends BaseTypeMapper[Long] with NumericType {
    def zero = 0
    def sqlType = java.sql.Types.BIGINT
    def setValue(v: Long, p: PositionedParameters) = p.setLong(v)
    def setOption(v: Option[Long], p: PositionedParameters) = p.setLongOption(v)
    def nextValue(r: PositionedResult) = r.nextLong
  }

  implicit object StringTypeMapper extends BaseTypeMapper[String] {
    def zero = ""
    def sqlType = java.sql.Types.VARCHAR
    def setValue(v: String, p: PositionedParameters) = p.setString(v)
    def setOption(v: Option[String], p: PositionedParameters) = p.setStringOption(v)
    def nextValue(r: PositionedResult) = r.nextString
    override def valueToSQLLiteral(value: String) = if(value eq null) "NULL" else {
      val sb = new StringBuilder
      sb append '\''
      for(c <- value) c match {
        case '\'' => sb append "''"
        case _ => sb append c
      }
      sb append '\''
      sb.toString
    }
  }

  implicit object TimeTypeMapper extends BaseTypeMapper[Time] {
    def zero = new Time(0, 0, 0)
    def sqlType = java.sql.Types.TIME
    def setValue(v: Time, p: PositionedParameters) = p.setTime(v)
    def setOption(v: Option[Time], p: PositionedParameters) = p.setTimeOption(v)
    def nextValue(r: PositionedResult) = r.nextTime
  }

  implicit object TimestampTypeMapper extends BaseTypeMapper[Timestamp] {
    def zero = new Timestamp(70, 0, 1, 0, 0, 0, 0)
    def sqlType = java.sql.Types.TIMESTAMP
    def setValue(v: Timestamp, p: PositionedParameters) = p.setTimestamp(v)
    def setOption(v: Option[Timestamp], p: PositionedParameters) = p.setTimestampOption(v)
    def nextValue(r: PositionedResult) = r.nextTimestamp
  }

  implicit def typeMapperToOptionTypeMapper[T](implicit t: TypeMapper[T]): OptionTypeMapper[T] = new OptionTypeMapper[T] {
    def zero = None
    def sqlType = t.sqlType
    def setValue(v: Option[T], p: PositionedParameters) = t.setOption(v, p)
    def setOption(v: Option[Option[T]], p: PositionedParameters) = t.setOption(v.getOrElse(None), p)
    def nextValue(r: PositionedResult) = t.nextOption(r)
    override def valueToSQLLiteral(value: Option[T]): String = value.map(t.valueToSQLLiteral).getOrElse("null")
  }

  object NullTypeMapper extends BaseTypeMapper[Null] {
    def zero = null
    def sqlType = java.sql.Types.NULL
    def setValue(v: Null, p: PositionedParameters) = p.setString(null)
    def setOption(v: Option[Null], p: PositionedParameters) = p.setString(null)
    def nextValue(r: PositionedResult) = { r.nextString; null }
    override def valueToSQLLiteral(value: Null) = "NULL"
  }
}
