package com.novocode.squery.combinator.basic

import java.sql.{Blob, Clob, Date, Time, Timestamp}
import com.novocode.squery.combinator.TypeMapperDelegate
import com.novocode.squery.session.{PositionedParameters, PositionedResult}

trait BasicTypeMapperDelegates {
  import BasicTypeMapperDelegates._
  val booleanTypeMapperDelegate = new BooleanTypeMapperDelegate
  val blobTypeMapperDelegate = new BlobTypeMapperDelegate
  val byteTypeMapperDelegate = new ByteTypeMapperDelegate
  val clobTypeMapperDelegate = new ClobTypeMapperDelegate
  val dateTypeMapperDelegate = new DateTypeMapperDelegate
  val doubleTypeMapperDelegate = new DoubleTypeMapperDelegate
  val floatTypeMapperDelegate = new FloatTypeMapperDelegate
  val intTypeMapperDelegate = new IntTypeMapperDelegate
  val longTypeMapperDelegate = new LongTypeMapperDelegate
  val stringTypeMapperDelegate = new StringTypeMapperDelegate
  val timeTypeMapperDelegate = new TimeTypeMapperDelegate
  val timestampTypeMapperDelegate = new TimestampTypeMapperDelegate
  val nullTypeMapperDelegate = new NullTypeMapperDelegate
}

object BasicTypeMapperDelegates {
  class BooleanTypeMapperDelegate extends TypeMapperDelegate[Boolean] {
    def zero = false
    def sqlType = java.sql.Types.BOOLEAN
    def setValue(v: Boolean, p: PositionedParameters) = p.setBoolean(v)
    def setOption(v: Option[Boolean], p: PositionedParameters) = p.setBooleanOption(v)
    def nextValue(r: PositionedResult) = r.nextBoolean
  }

  class BlobTypeMapperDelegate extends TypeMapperDelegate[Blob] {
    def zero = null
    def sqlType = java.sql.Types.BLOB
    def setValue(v: Blob, p: PositionedParameters) = p.setBlob(v)
    def setOption(v: Option[Blob], p: PositionedParameters) = p.setBlobOption(v)
    def nextValue(r: PositionedResult) = r.nextBlob
  }

  class ByteTypeMapperDelegate extends TypeMapperDelegate[Byte] {
    def zero = 0
    def sqlType = java.sql.Types.TINYINT
    def setValue(v: Byte, p: PositionedParameters) = p.setByte(v)
    def setOption(v: Option[Byte], p: PositionedParameters) = p.setByteOption(v)
    def nextValue(r: PositionedResult) = r.nextByte
  }

  class ClobTypeMapperDelegate extends TypeMapperDelegate[Clob] {
    def zero = null
    def sqlType = java.sql.Types.CLOB
    def setValue(v: Clob, p: PositionedParameters) = p.setClob(v)
    def setOption(v: Option[Clob], p: PositionedParameters) = p.setClobOption(v)
    def nextValue(r: PositionedResult) = r.nextClob
  }

  class DateTypeMapperDelegate extends TypeMapperDelegate[Date] {
    def zero = new Date(70, 0, 1)
    def sqlType = java.sql.Types.DATE
    def setValue(v: Date, p: PositionedParameters) = p.setDate(v)
    def setOption(v: Option[Date], p: PositionedParameters) = p.setDateOption(v)
    def nextValue(r: PositionedResult) = r.nextDate
  }

  class DoubleTypeMapperDelegate extends TypeMapperDelegate[Double] {
    def zero = 0
    def sqlType = java.sql.Types.DOUBLE
    def setValue(v: Double, p: PositionedParameters) = p.setDouble(v)
    def setOption(v: Option[Double], p: PositionedParameters) = p.setDoubleOption(v)
    def nextValue(r: PositionedResult) = r.nextDouble
  }

  class FloatTypeMapperDelegate extends TypeMapperDelegate[Float] {
    def zero = 0
    def sqlType = java.sql.Types.FLOAT
    def setValue(v: Float, p: PositionedParameters) = p.setFloat(v)
    def setOption(v: Option[Float], p: PositionedParameters) = p.setFloatOption(v)
    def nextValue(r: PositionedResult) = r.nextFloat
  }

  class IntTypeMapperDelegate extends TypeMapperDelegate[Int] {
    def zero = 0
    def sqlType = java.sql.Types.INTEGER
    def setValue(v: Int, p: PositionedParameters) = p.setInt(v)
    def setOption(v: Option[Int], p: PositionedParameters) = p.setIntOption(v)
    def nextValue(r: PositionedResult) = r.nextInt
  }

  class LongTypeMapperDelegate extends TypeMapperDelegate[Long] {
    def zero = 0
    def sqlType = java.sql.Types.BIGINT
    def setValue(v: Long, p: PositionedParameters) = p.setLong(v)
    def setOption(v: Option[Long], p: PositionedParameters) = p.setLongOption(v)
    def nextValue(r: PositionedResult) = r.nextLong
  }

  class StringTypeMapperDelegate extends TypeMapperDelegate[String] {
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

  class TimeTypeMapperDelegate extends TypeMapperDelegate[Time] {
    def zero = new Time(0, 0, 0)
    def sqlType = java.sql.Types.TIME
    def setValue(v: Time, p: PositionedParameters) = p.setTime(v)
    def setOption(v: Option[Time], p: PositionedParameters) = p.setTimeOption(v)
    def nextValue(r: PositionedResult) = r.nextTime
  }

  class TimestampTypeMapperDelegate extends TypeMapperDelegate[Timestamp] {
    def zero = new Timestamp(70, 0, 1, 0, 0, 0, 0)
    def sqlType = java.sql.Types.TIMESTAMP
    def setValue(v: Timestamp, p: PositionedParameters) = p.setTimestamp(v)
    def setOption(v: Option[Timestamp], p: PositionedParameters) = p.setTimestampOption(v)
    def nextValue(r: PositionedResult) = r.nextTimestamp
  }

  class NullTypeMapperDelegate extends TypeMapperDelegate[Null] {
    def zero = null
    def sqlType = java.sql.Types.NULL
    def setValue(v: Null, p: PositionedParameters) = p.setString(null)
    def setOption(v: Option[Null], p: PositionedParameters) = p.setString(null)
    def nextValue(r: PositionedResult) = { r.nextString; null }
    override def valueToSQLLiteral(value: Null) = "NULL"
  }
}
