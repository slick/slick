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

object TypeMapper {
  implicit object BooleanTypeMapper extends TypeMapper[Boolean] {
    def zero = false
    def sqlType = java.sql.Types.BOOLEAN
    def setValue(v: Boolean, p: PositionedParameters) = p.setBoolean(v)
    def setOption(v: Option[Boolean], p: PositionedParameters) = p.setBooleanOption(v)
    def nextValue(r: PositionedResult) = r.nextBoolean
  }

  implicit object BlobTypeMapper extends TypeMapper[Blob] {
    def zero = null
    def sqlType = java.sql.Types.BLOB
    def setValue(v: Blob, p: PositionedParameters) = p.setBlob(v)
    def setOption(v: Option[Blob], p: PositionedParameters) = p.setBlobOption(v)
    def nextValue(r: PositionedResult) = r.nextBlob
  }

  implicit object ByteTypeMapper extends TypeMapper[Byte] {
    def zero = 0
    def sqlType = java.sql.Types.TINYINT
    def setValue(v: Byte, p: PositionedParameters) = p.setByte(v)
    def setOption(v: Option[Byte], p: PositionedParameters) = p.setByteOption(v)
    def nextValue(r: PositionedResult) = r.nextByte
  }

  implicit object ClobTypeMapper extends TypeMapper[Clob] {
    def zero = null
    def sqlType = java.sql.Types.CLOB
    def setValue(v: Clob, p: PositionedParameters) = p.setClob(v)
    def setOption(v: Option[Clob], p: PositionedParameters) = p.setClobOption(v)
    def nextValue(r: PositionedResult) = r.nextClob
  }

  implicit object DateTypeMapper extends TypeMapper[Date] {
    def zero = new Date(70, 0, 1)
    def sqlType = java.sql.Types.DATE
    def setValue(v: Date, p: PositionedParameters) = p.setDate(v)
    def setOption(v: Option[Date], p: PositionedParameters) = p.setDateOption(v)
    def nextValue(r: PositionedResult) = r.nextDate
  }

  implicit object DoubleTypeMapper extends TypeMapper[Double] {
    def zero = 0
    def sqlType = java.sql.Types.DOUBLE
    def setValue(v: Double, p: PositionedParameters) = p.setDouble(v)
    def setOption(v: Option[Double], p: PositionedParameters) = p.setDoubleOption(v)
    def nextValue(r: PositionedResult) = r.nextDouble
  }

  implicit object FloatTypeMapper extends TypeMapper[Float] {
    def zero = 0
    def sqlType = java.sql.Types.FLOAT
    def setValue(v: Float, p: PositionedParameters) = p.setFloat(v)
    def setOption(v: Option[Float], p: PositionedParameters) = p.setFloatOption(v)
    def nextValue(r: PositionedResult) = r.nextFloat
  }

  implicit object IntTypeMapper extends TypeMapper[Int] {
    def zero = 0
    def sqlType = java.sql.Types.INTEGER
    def setValue(v: Int, p: PositionedParameters) = p.setInt(v)
    def setOption(v: Option[Int], p: PositionedParameters) = p.setIntOption(v)
    def nextValue(r: PositionedResult) = r.nextInt
  }

  implicit object LongTypeMapper extends TypeMapper[Long] {
    def zero = 0
    def sqlType = java.sql.Types.INTEGER
    def setValue(v: Long, p: PositionedParameters) = p.setLong(v)
    def setOption(v: Option[Long], p: PositionedParameters) = p.setLongOption(v)
    def nextValue(r: PositionedResult) = r.nextLong
  }

  implicit object StringTypeMapper extends TypeMapper[String] {
    def zero = ""
    def sqlType = java.sql.Types.VARCHAR
    def setValue(v: String, p: PositionedParameters) = p.setString(v)
    def setOption(v: Option[String], p: PositionedParameters) = p.setStringOption(v)
    def nextValue(r: PositionedResult) = r.nextString
    override def valueToSQLLiteral(value: String) = createStringLiteral(value, new StringBuilder).toString
    def createStringLiteral(s: String, sb: StringBuilder) = {
      sb append '\''
      for(c <- s) c match {
        case '\'' => sb append "''"
        case _ => sb append c
      }
      sb append '\''
      sb
    }
  }

  implicit object TimeTypeMapper extends TypeMapper[Time] {
    def zero = new Time(0, 0, 0)
    def sqlType = java.sql.Types.TIME
    def setValue(v: Time, p: PositionedParameters) = p.setTime(v)
    def setOption(v: Option[Time], p: PositionedParameters) = p.setTimeOption(v)
    def nextValue(r: PositionedResult) = r.nextTime
  }

  implicit object TimestampTypeMapper extends TypeMapper[Timestamp] {
    def zero = new Timestamp(70, 0, 1, 0, 0, 0, 0)
    def sqlType = java.sql.Types.TIMESTAMP
    def setValue(v: Timestamp, p: PositionedParameters) = p.setTimestamp(v)
    def setOption(v: Option[Timestamp], p: PositionedParameters) = p.setTimestampOption(v)
    def nextValue(r: PositionedResult) = r.nextTimestamp
  }

  implicit def typeMapperToOptionTypeMapper[T](implicit t: TypeMapper[T]): TypeMapper[Option[T]] = new TypeMapper[Option[T]] {
    def zero = None
    def sqlType = t.sqlType
    def setValue(v: Option[T], p: PositionedParameters) = t.setOption(v, p)
    def setOption(v: Option[Option[T]], p: PositionedParameters) = t.setOption(v.getOrElse(None), p)
    def nextValue(r: PositionedResult) = t.nextOption(r)
    override def valueToSQLLiteral(value: Option[T]): String = value.map(t.valueToSQLLiteral).getOrElse("null")
  }
}
