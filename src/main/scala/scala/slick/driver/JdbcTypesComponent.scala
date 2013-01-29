package scala.slick.driver

import java.sql.{Blob, Clob, Date, Time, Timestamp}
import scala.slick.SlickException
import scala.slick.ast.{OptionType, NumericTypedType, BaseTypedType, StaticType, Type}
import scala.slick.jdbc.{PositionedParameters, PositionedResult, JdbcType}
import java.util.UUID

trait JdbcTypesComponent { driver: JdbcDriver =>

  type TypeInfo = JdbcType[Any /* it's really _ but we'd have to cast it to Any anyway */]

  def typeInfoFor(t: Type): TypeInfo = ((t match {
    case tmd: JdbcType[_] => tmd
    case StaticType.Boolean => columnTypes.booleanJdbcType
    case StaticType.Char => columnTypes.charJdbcType
    case StaticType.Int => columnTypes.intJdbcType
    case StaticType.Long => columnTypes.longJdbcType
    case StaticType.Null => columnTypes.nullJdbcType
    case StaticType.String => columnTypes.stringJdbcType
    case StaticType.Unit => columnTypes.unitJdbcType
    case o: OptionType => typeInfoFor(o.elementType).optionType
    case t => throw new SlickException("JdbcProfile has no TypeInfo for type "+t)
  }): JdbcType[_]).asInstanceOf[JdbcType[Any]]

  def defaultSqlTypeName(tmd: JdbcType[_]): String = tmd.sqlType match {
    case java.sql.Types.VARCHAR => "VARCHAR(254)"
    case java.sql.Types.DECIMAL => "DECIMAL(21,2)"
    case t => JdbcType.typeNames.getOrElse(t,
      throw new SlickException("No SQL type name found in java.sql.Types for code "+t))
  }

  trait DriverJdbcType[T] extends JdbcType[T] with BaseTypedType[T] {
    def sqlTypeName: String = driver.defaultSqlTypeName(this)
  }

  class JdbcTypes {
    val booleanJdbcType = new BooleanJdbcType
    val blobJdbcType = new BlobJdbcType
    val byteJdbcType = new ByteJdbcType
    val byteArrayJdbcType = new ByteArrayJdbcType
    val charJdbcType = new CharJdbcType
    val clobJdbcType = new ClobJdbcType
    val dateJdbcType = new DateJdbcType
    val doubleJdbcType = new DoubleJdbcType
    val floatJdbcType = new FloatJdbcType
    val intJdbcType = new IntJdbcType
    val longJdbcType = new LongJdbcType
    val shortJdbcType = new ShortJdbcType
    val stringJdbcType = new StringJdbcType
    val timeJdbcType = new TimeJdbcType
    val timestampJdbcType = new TimestampJdbcType
    val unitJdbcType = new UnitJdbcType
    val uuidJdbcType = new UUIDJdbcType
    val bigDecimalJdbcType = new BigDecimalJdbcType
    val nullJdbcType = new NullJdbcType

    class BooleanJdbcType extends DriverJdbcType[Boolean] {
      def zero = false
      def sqlType = java.sql.Types.BOOLEAN
      def setValue(v: Boolean, p: PositionedParameters) = p.setBoolean(v)
      def setOption(v: Option[Boolean], p: PositionedParameters) = p.setBooleanOption(v)
      def nextValue(r: PositionedResult) = r.nextBoolean
      def updateValue(v: Boolean, r: PositionedResult) = r.updateBoolean(v)
    }

    class BlobJdbcType extends DriverJdbcType[Blob] {
      def zero = null
      def sqlType = java.sql.Types.BLOB
      def setValue(v: Blob, p: PositionedParameters) = p.setBlob(v)
      def setOption(v: Option[Blob], p: PositionedParameters) = p.setBlobOption(v)
      def nextValue(r: PositionedResult) = r.nextBlob
      def updateValue(v: Blob, r: PositionedResult) = r.updateBlob(v)
      override def valueToSQLLiteral(value: Blob) =
        throw new SlickException("Blob does not have a literal representation")
    }

    class ByteJdbcType extends DriverJdbcType[Byte] with NumericTypedType {
      def zero = 0
      def sqlType = java.sql.Types.TINYINT
      def setValue(v: Byte, p: PositionedParameters) = p.setByte(v)
      def setOption(v: Option[Byte], p: PositionedParameters) = p.setByteOption(v)
      def nextValue(r: PositionedResult) = r.nextByte
      def updateValue(v: Byte, r: PositionedResult) = r.updateByte(v)
    }

    class ByteArrayJdbcType extends DriverJdbcType[Array[Byte]] {
      val zero = new Array[Byte](0)
      val sqlType = java.sql.Types.BLOB
      def setValue(v: Array[Byte], p: PositionedParameters) = p.setBytes(v)
      def setOption(v: Option[Array[Byte]], p: PositionedParameters) = p.setBytesOption(v)
      def nextValue(r: PositionedResult) = r.nextBytes
      def updateValue(v: Array[Byte], r: PositionedResult) = r.updateBytes(v)
      override def valueToSQLLiteral(value: Array[Byte]) =
        throw new SlickException("Array[Byte] does not have a literal representation")
    }

    class ClobJdbcType extends DriverJdbcType[Clob] {
      def zero = null
      def sqlType = java.sql.Types.CLOB
      def setValue(v: Clob, p: PositionedParameters) = p.setClob(v)
      def setOption(v: Option[Clob], p: PositionedParameters) = p.setClobOption(v)
      def nextValue(r: PositionedResult) = r.nextClob
      def updateValue(v: Clob, r: PositionedResult) = r.updateClob(v)
    }

    class CharJdbcType extends JdbcType[Char] with BaseTypedType[Char] {
      def zero = ' '
      def sqlType = java.sql.Types.CHAR
      def sqlTypeName = "CHAR(1)"
      def setValue(v: Char, p: PositionedParameters) = stringJdbcType.setValue(String.valueOf(v), p)
      def setOption(v: Option[Char], p: PositionedParameters) = stringJdbcType.setOption(v.map(String.valueOf), p)
      def nextValue(r: PositionedResult) = {
        val s = stringJdbcType.nextValue(r)
        if(s.isEmpty) zero else s.charAt(0)
      }
      def updateValue(v: Char, r: PositionedResult) = stringJdbcType.updateValue(String.valueOf(v), r)
      override def valueToSQLLiteral(v: Char) = stringJdbcType.valueToSQLLiteral(String.valueOf(v))
    }

    class DateJdbcType extends DriverJdbcType[Date] {
      def zero = new Date(0L)
      def sqlType = java.sql.Types.DATE
      def setValue(v: Date, p: PositionedParameters) = p.setDate(v)
      def setOption(v: Option[Date], p: PositionedParameters) = p.setDateOption(v)
      def nextValue(r: PositionedResult) = r.nextDate
      def updateValue(v: Date, r: PositionedResult) = r.updateDate(v)
      override def valueToSQLLiteral(value: Date) = "{d '"+value.toString+"'}"
    }

    class DoubleJdbcType extends DriverJdbcType[Double] with NumericTypedType {
      def zero = 0
      def sqlType = java.sql.Types.DOUBLE
      def setValue(v: Double, p: PositionedParameters) = p.setDouble(v)
      def setOption(v: Option[Double], p: PositionedParameters) = p.setDoubleOption(v)
      def nextValue(r: PositionedResult) = r.nextDouble
      def updateValue(v: Double, r: PositionedResult) = r.updateDouble(v)
    }

    class FloatJdbcType extends DriverJdbcType[Float] with NumericTypedType {
      def zero = 0
      def sqlType = java.sql.Types.FLOAT
      def setValue(v: Float, p: PositionedParameters) = p.setFloat(v)
      def setOption(v: Option[Float], p: PositionedParameters) = p.setFloatOption(v)
      def nextValue(r: PositionedResult) = r.nextFloat
      def updateValue(v: Float, r: PositionedResult) = r.updateFloat(v)
    }

    class IntJdbcType extends DriverJdbcType[Int] with NumericTypedType {
      def zero = 0
      def sqlType = java.sql.Types.INTEGER
      def setValue(v: Int, p: PositionedParameters) = p.setInt(v)
      def setOption(v: Option[Int], p: PositionedParameters) = p.setIntOption(v)
      def nextValue(r: PositionedResult) = r.nextInt
      def updateValue(v: Int, r: PositionedResult) = r.updateInt(v)
    }

    class LongJdbcType extends DriverJdbcType[Long] with NumericTypedType {
      def zero = 0
      def sqlType = java.sql.Types.BIGINT
      def setValue(v: Long, p: PositionedParameters) = p.setLong(v)
      def setOption(v: Option[Long], p: PositionedParameters) = p.setLongOption(v)
      def nextValue(r: PositionedResult) = r.nextLong
      def updateValue(v: Long, r: PositionedResult) = r.updateLong(v)
    }

    class ShortJdbcType extends DriverJdbcType[Short] with NumericTypedType {
      def zero = 0
      def sqlType = java.sql.Types.SMALLINT
      def setValue(v: Short, p: PositionedParameters) = p.setShort(v)
      def setOption(v: Option[Short], p: PositionedParameters) = p.setShortOption(v)
      def nextValue(r: PositionedResult) = r.nextShort
      def updateValue(v: Short, r: PositionedResult) = r.updateShort(v)
    }

    class StringJdbcType extends DriverJdbcType[String] {
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

    class TimeJdbcType extends DriverJdbcType[Time] {
      def zero = new Time(0L)
      def sqlType = java.sql.Types.TIME
      def setValue(v: Time, p: PositionedParameters) = p.setTime(v)
      def setOption(v: Option[Time], p: PositionedParameters) = p.setTimeOption(v)
      def nextValue(r: PositionedResult) = r.nextTime
      def updateValue(v: Time, r: PositionedResult) = r.updateTime(v)
      override def valueToSQLLiteral(value: Time) = "{t '"+value.toString+"'}"
    }

    class TimestampJdbcType extends DriverJdbcType[Timestamp] {
      def zero = new Timestamp(0L)
      def sqlType = java.sql.Types.TIMESTAMP
      def setValue(v: Timestamp, p: PositionedParameters) = p.setTimestamp(v)
      def setOption(v: Option[Timestamp], p: PositionedParameters) = p.setTimestampOption(v)
      def nextValue(r: PositionedResult) = r.nextTimestamp
      def updateValue(v: Timestamp, r: PositionedResult) = r.updateTimestamp(v)
      override def valueToSQLLiteral(value: Timestamp) = "{ts '"+value.toString+"'}"
    }

    class UnitJdbcType extends DriverJdbcType[Unit] {
      def zero = ()
      def sqlType = java.sql.Types.INTEGER
      def setValue(v: Unit, p: PositionedParameters) = p.setInt(1)
      def setOption(v: Option[Unit], p: PositionedParameters) = p.setIntOption(v.map(_ => 1))
      def nextValue(r: PositionedResult) = { r.nextInt; () }
      def updateValue(v: Unit, r: PositionedResult) = r.updateInt(1)
      override def valueToSQLLiteral(value: Unit) = "1"
    }

    class UUIDJdbcType extends DriverJdbcType[UUID] {
      def zero = new UUID(0, 0)
      def sqlType = java.sql.Types.OTHER
      def setValue(v: UUID, p: PositionedParameters) = p.setBytes(toBytes(v))
      def setOption(v: Option[UUID], p: PositionedParameters) =
        if(v == None) p.setNull(sqlType) else p.setBytes(toBytes(v.get))
      def nextValue(r: PositionedResult) = fromBytes(r.nextBytes())
      def updateValue(v: UUID, r: PositionedResult) = r.updateBytes(toBytes(v))
      override def valueToSQLLiteral(value: UUID): String =
        throw new SlickException("UUID does not support a literal representation")
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

    class BigDecimalJdbcType extends DriverJdbcType[BigDecimal] with NumericTypedType {
      def zero = BigDecimal(0)
      def sqlType = java.sql.Types.DECIMAL
      def setValue(v: BigDecimal, p: PositionedParameters) = p.setBigDecimal(v)
      def setOption(v: Option[BigDecimal], p: PositionedParameters) = p.setBigDecimalOption(v)
      def nextValue(r: PositionedResult) = r.nextBigDecimal
      def updateValue(v: BigDecimal, r: PositionedResult) = r.updateBigDecimal(v)
    }

    class NullJdbcType extends DriverJdbcType[Null] {
      def zero = null
      def sqlType = java.sql.Types.NULL
      def setValue(v: Null, p: PositionedParameters) = p.setString(null)
      def setOption(v: Option[Null], p: PositionedParameters) = p.setString(null)
      def nextValue(r: PositionedResult) = { r.nextString; null }
      def updateValue(v: Null, r: PositionedResult) = r.updateNull()
      override def valueToSQLLiteral(value: Null) = "NULL"
    }
  }

  class ImplicitJdbcTypes {
    implicit def booleanJdbcType = columnTypes.booleanJdbcType
    implicit def blobJdbcType = columnTypes.blobJdbcType
    implicit def byteJdbcType = columnTypes.byteJdbcType
    implicit def byteArrayJdbcType = columnTypes.byteArrayJdbcType
    implicit def charJdbcType = columnTypes.charJdbcType
    implicit def clobJdbcType = columnTypes.clobJdbcType
    implicit def dateJdbcType = columnTypes.dateJdbcType
    implicit def doubleJdbcType = columnTypes.doubleJdbcType
    implicit def floatJdbcType = columnTypes.floatJdbcType
    implicit def intJdbcType = columnTypes.intJdbcType
    implicit def longJdbcType = columnTypes.longJdbcType
    implicit def shortJdbcType = columnTypes.shortJdbcType
    implicit def stringJdbcType = columnTypes.stringJdbcType
    implicit def timeJdbcType = columnTypes.timeJdbcType
    implicit def timestampJdbcType = columnTypes.timestampJdbcType
    implicit def unitJdbcType = columnTypes.unitJdbcType
    implicit def uuidJdbcType = columnTypes.uuidJdbcType
    implicit def bigDecimalJdbcType = columnTypes.bigDecimalJdbcType
  }
}
