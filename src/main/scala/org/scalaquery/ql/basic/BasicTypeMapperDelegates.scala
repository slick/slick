package org.scalaquery.ql.basic

import java.sql.{Blob, Clob, Date, Time, Timestamp}
import org.scalaquery.SQueryException
import org.scalaquery.ql.TypeMapperDelegate
import org.scalaquery.session.{PositionedParameters, PositionedResult}
import java.util.UUID

trait BasicTypeMapperDelegates {
  import BasicTypeMapperDelegates._
  val booleanTypeMapperDelegate = new BooleanTypeMapperDelegate
  val blobTypeMapperDelegate = new BlobTypeMapperDelegate
  val byteTypeMapperDelegate = new ByteTypeMapperDelegate
  val byteArrayTypeMapperDelegate = new ByteArrayTypeMapperDelegate
  val clobTypeMapperDelegate = new ClobTypeMapperDelegate
  val dateTypeMapperDelegate = new DateTypeMapperDelegate
  val doubleTypeMapperDelegate = new DoubleTypeMapperDelegate
  val floatTypeMapperDelegate = new FloatTypeMapperDelegate
  val intTypeMapperDelegate = new IntTypeMapperDelegate
  val longTypeMapperDelegate = new LongTypeMapperDelegate
  val shortTypeMapperDelegate = new ShortTypeMapperDelegate
  val stringTypeMapperDelegate = new StringTypeMapperDelegate
  val timeTypeMapperDelegate = new TimeTypeMapperDelegate
  val timestampTypeMapperDelegate = new TimestampTypeMapperDelegate
  val unitTypeMapperDelegate = new UnitTypeMapperDelegate
  val uuidTypeMapperDelegate = new UUIDTypeMapperDelegate
  val bigDecimalTypeMapperDelegate = new BigDecimalTypeMapperDelegate
  val nullTypeMapperDelegate = new NullTypeMapperDelegate
}

object BasicTypeMapperDelegates {
  class BooleanTypeMapperDelegate extends TypeMapperDelegate[Boolean] {
    def zero = false
    def sqlType = java.sql.Types.BOOLEAN
    def setValue(v: Boolean, p: PositionedParameters) = p.setBoolean(v)
    def setOption(v: Option[Boolean], p: PositionedParameters) = p.setBooleanOption(v)
    def nextValue(r: PositionedResult) = r.nextBoolean
    def updateValue(v: Boolean, r: PositionedResult) = r.updateBoolean(v)
  }

  class BlobTypeMapperDelegate extends TypeMapperDelegate[Blob] {
    def zero = null
    def sqlType = java.sql.Types.BLOB
    def setValue(v: Blob, p: PositionedParameters) = p.setBlob(v)
    def setOption(v: Option[Blob], p: PositionedParameters) = p.setBlobOption(v)
    def nextValue(r: PositionedResult) = r.nextBlob
    def updateValue(v: Blob, r: PositionedResult) = r.updateBlob(v)
    override def valueToSQLLiteral(value: Blob) =
      throw new SQueryException("Blob does not have a literal representation")
  }

  class ByteTypeMapperDelegate extends TypeMapperDelegate[Byte] {
    def zero = 0
    def sqlType = java.sql.Types.TINYINT
    def setValue(v: Byte, p: PositionedParameters) = p.setByte(v)
    def setOption(v: Option[Byte], p: PositionedParameters) = p.setByteOption(v)
    def nextValue(r: PositionedResult) = r.nextByte
    def updateValue(v: Byte, r: PositionedResult) = r.updateByte(v)
  }

  class ByteArrayTypeMapperDelegate extends TypeMapperDelegate[Array[Byte]] {
    val zero = new Array[Byte](0)
    val sqlType = java.sql.Types.BLOB
    def setValue(v: Array[Byte], p: PositionedParameters) = p.setBytes(v)
    def setOption(v: Option[Array[Byte]], p: PositionedParameters) = p.setBytesOption(v)
    def nextValue(r: PositionedResult) = r.nextBytes
    def updateValue(v: Array[Byte], r: PositionedResult) = r.updateBytes(v)
    override def valueToSQLLiteral(value: Array[Byte]) =
      throw new SQueryException("Array[Byte] does not have a literal representation")
  }

  class ClobTypeMapperDelegate extends TypeMapperDelegate[Clob] {
    def zero = null
    def sqlType = java.sql.Types.CLOB
    def setValue(v: Clob, p: PositionedParameters) = p.setClob(v)
    def setOption(v: Option[Clob], p: PositionedParameters) = p.setClobOption(v)
    def nextValue(r: PositionedResult) = r.nextClob
    def updateValue(v: Clob, r: PositionedResult) = r.updateClob(v)
  }

  class DateTypeMapperDelegate extends TypeMapperDelegate[Date] {
    def zero = new Date(0L)
    def sqlType = java.sql.Types.DATE
    def setValue(v: Date, p: PositionedParameters) = p.setDate(v)
    def setOption(v: Option[Date], p: PositionedParameters) = p.setDateOption(v)
    def nextValue(r: PositionedResult) = r.nextDate
    def updateValue(v: Date, r: PositionedResult) = r.updateDate(v)
    override def valueToSQLLiteral(value: Date) = "{d '"+value.toString+"'}"
  }

  class DoubleTypeMapperDelegate extends TypeMapperDelegate[Double] {
    def zero = 0
    def sqlType = java.sql.Types.DOUBLE
    def setValue(v: Double, p: PositionedParameters) = p.setDouble(v)
    def setOption(v: Option[Double], p: PositionedParameters) = p.setDoubleOption(v)
    def nextValue(r: PositionedResult) = r.nextDouble
    def updateValue(v: Double, r: PositionedResult) = r.updateDouble(v)
  }

  class FloatTypeMapperDelegate extends TypeMapperDelegate[Float] {
    def zero = 0
    def sqlType = java.sql.Types.FLOAT
    def setValue(v: Float, p: PositionedParameters) = p.setFloat(v)
    def setOption(v: Option[Float], p: PositionedParameters) = p.setFloatOption(v)
    def nextValue(r: PositionedResult) = r.nextFloat
    def updateValue(v: Float, r: PositionedResult) = r.updateFloat(v)
  }

  class IntTypeMapperDelegate extends TypeMapperDelegate[Int] {
    def zero = 0
    def sqlType = java.sql.Types.INTEGER
    def setValue(v: Int, p: PositionedParameters) = p.setInt(v)
    def setOption(v: Option[Int], p: PositionedParameters) = p.setIntOption(v)
    def nextValue(r: PositionedResult) = r.nextInt
    def updateValue(v: Int, r: PositionedResult) = r.updateInt(v)
  }

  class LongTypeMapperDelegate extends TypeMapperDelegate[Long] {
    def zero = 0
    def sqlType = java.sql.Types.BIGINT
    def setValue(v: Long, p: PositionedParameters) = p.setLong(v)
    def setOption(v: Option[Long], p: PositionedParameters) = p.setLongOption(v)
    def nextValue(r: PositionedResult) = r.nextLong
    def updateValue(v: Long, r: PositionedResult) = r.updateLong(v)
  }

  class ShortTypeMapperDelegate extends TypeMapperDelegate[Short] {
    def zero = 0
    def sqlType = java.sql.Types.SMALLINT
    def setValue(v: Short, p: PositionedParameters) = p.setShort(v)
    def setOption(v: Option[Short], p: PositionedParameters) = p.setShortOption(v)
    def nextValue(r: PositionedResult) = r.nextShort
    def updateValue(v: Short, r: PositionedResult) = r.updateShort(v)
  }

  class StringTypeMapperDelegate extends TypeMapperDelegate[String] {
    def zero = ""
    def sqlType = java.sql.Types.VARCHAR
    def setValue(v: String, p: PositionedParameters) = p.setString(v)
    def setOption(v: Option[String], p: PositionedParameters) = p.setStringOption(v)
    def nextValue(r: PositionedResult) = r.nextString
    def updateValue(v: String, r: PositionedResult) = r.updateString(v)
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
    def zero = new Time(0L)
    def sqlType = java.sql.Types.TIME
    def setValue(v: Time, p: PositionedParameters) = p.setTime(v)
    def setOption(v: Option[Time], p: PositionedParameters) = p.setTimeOption(v)
    def nextValue(r: PositionedResult) = r.nextTime
    def updateValue(v: Time, r: PositionedResult) = r.updateTime(v)
    override def valueToSQLLiteral(value: Time) = "{t '"+value.toString+"'}"
  }

  class TimestampTypeMapperDelegate extends TypeMapperDelegate[Timestamp] {
    def zero = new Timestamp(0L)
    def sqlType = java.sql.Types.TIMESTAMP
    def setValue(v: Timestamp, p: PositionedParameters) = p.setTimestamp(v)
    def setOption(v: Option[Timestamp], p: PositionedParameters) = p.setTimestampOption(v)
    def nextValue(r: PositionedResult) = r.nextTimestamp
    def updateValue(v: Timestamp, r: PositionedResult) = r.updateTimestamp(v)
    override def valueToSQLLiteral(value: Timestamp) = "{ts '"+value.toString+"'}"
  }

  class UnitTypeMapperDelegate extends TypeMapperDelegate[Unit] {
    def zero = ()
    def sqlType = java.sql.Types.INTEGER
    def setValue(v: Unit, p: PositionedParameters) = p.setInt(1)
    def setOption(v: Option[Unit], p: PositionedParameters) = p.setIntOption(v.map(_ => 1))
    def nextValue(r: PositionedResult) = { r.nextInt; () }
    def updateValue(v: Unit, r: PositionedResult) = r.updateInt(1)
    override def valueToSQLLiteral(value: Unit) = "1"
  }

  class UUIDTypeMapperDelegate extends TypeMapperDelegate[UUID] {
    def zero = new UUID(0, 0)
    def sqlType = java.sql.Types.OTHER
    override def sqlTypeName = "UUID"
    def setValue(v: UUID, p: PositionedParameters) = p.setBytes(toBytes(v))
    def setOption(v: Option[UUID], p: PositionedParameters) =
      if(v == None) p.setNull(sqlType) else p.setBytes(toBytes(v.get))
    def nextValue(r: PositionedResult) = fromBytes(r.nextBytes())
    def updateValue(v: UUID, r: PositionedResult) = r.updateBytes(toBytes(v))
    override def valueToSQLLiteral(value: UUID): String =
      throw new SQueryException("UUID does not support a literal representation")
    def toBytes(uuid: UUID) = if(uuid eq null) null else {
      val msb = uuid.getMostSignificantBits
      val lsb = uuid.getLeastSignificantBits
      val buff = new Array[Byte](16)
      var i = 0
      while(i < 8) {
        buff(i) = ((msb >> (8 * (7 - i))) & 255).toByte;
        buff(8 + i) = ((lsb >> (8 * (7 - i))) & 255).toByte;
        i += 1
      }
      buff
    }
    def fromBytes(data: Array[Byte]) = if(data eq null) null else {
      var msb = 0L
      var lsb = 0L
      var i = 0
      while(i < 8) {
        msb = (msb << 8) | (data(i) & 0xff)
        i += 1
      }
      while(i < 16) {
        lsb = (lsb << 8) | (data(i) & 0xff)
        i += 1
      }
      new UUID(msb, lsb)
    }
  }

  class BigDecimalTypeMapperDelegate extends TypeMapperDelegate[BigDecimal] {
    def zero = BigDecimal(0)
    def sqlType = java.sql.Types.DECIMAL
    def setValue(v: BigDecimal, p: PositionedParameters) = p.setBigDecimal(v)
    def setOption(v: Option[BigDecimal], p: PositionedParameters) = p.setBigDecimalOption(v)
    def nextValue(r: PositionedResult) = r.nextBigDecimal
    def updateValue(v: BigDecimal, r: PositionedResult) = r.updateBigDecimal(v)
  }

  class NullTypeMapperDelegate extends TypeMapperDelegate[Null] {
    def zero = null
    def sqlType = java.sql.Types.NULL
    def setValue(v: Null, p: PositionedParameters) = p.setString(null)
    def setOption(v: Option[Null], p: PositionedParameters) = p.setString(null)
    def nextValue(r: PositionedResult) = { r.nextString; null }
    def updateValue(v: Null, r: PositionedResult) = r.updateNull()
    override def valueToSQLLiteral(value: Null) = "NULL"
  }
}
