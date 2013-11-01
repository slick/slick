package scala.slick.driver

import java.sql.{Blob, Clob, Date, Time, Timestamp}
import java.util.UUID
import scala.slick.SlickException
import scala.slick.ast.{ScalaBaseType, OptionType, NumericTypedType, BaseTypedType, Type}
import scala.slick.jdbc.{PositionedParameters, PositionedResult, JdbcType}
import scala.slick.profile.RelationalTypesComponent
import scala.reflect.ClassTag

trait JdbcTypesComponent extends RelationalTypesComponent { driver: JdbcDriver =>

  type TypeInfo = JdbcType[Any /* it's really _ but we'd have to cast it to Any anyway */]

  def typeInfoFor(t: Type): TypeInfo = ((t match {
    case tmd: JdbcType[_] => tmd
    case ScalaBaseType.booleanType => columnTypes.booleanJdbcType
    case ScalaBaseType.bigDecimalType => columnTypes.bigDecimalJdbcType
    case ScalaBaseType.byteType => columnTypes.byteJdbcType
    case ScalaBaseType.charType => columnTypes.charJdbcType
    case ScalaBaseType.doubleType => columnTypes.doubleJdbcType
    case ScalaBaseType.floatType => columnTypes.floatJdbcType
    case ScalaBaseType.intType => columnTypes.intJdbcType
    case ScalaBaseType.longType => columnTypes.longJdbcType
    case ScalaBaseType.nullType => columnTypes.nullJdbcType
    case ScalaBaseType.shortType => columnTypes.shortJdbcType
    case ScalaBaseType.stringType => columnTypes.stringJdbcType
    case o: OptionType => typeInfoFor(o.elementType).optionType
    case t => throw new SlickException("JdbcProfile has no TypeInfo for type "+t)
  }): JdbcType[_]).asInstanceOf[JdbcType[Any]]

  def defaultSqlTypeName(tmd: JdbcType[_]): String = tmd.sqlType match {
    case java.sql.Types.VARCHAR => "VARCHAR(254)"
    case java.sql.Types.DECIMAL => "DECIMAL(21,2)"
    case t => JdbcType.typeNames.getOrElse(t,
      throw new SlickException("No SQL type name found in java.sql.Types for code "+t))
  }

  abstract class DriverJdbcType[T : ClassTag] extends JdbcType[T] with BaseTypedType[T] {
    def scalaType = ScalaBaseType[T]
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
    val uuidJdbcType = new UUIDJdbcType
    val bigDecimalJdbcType = new BigDecimalJdbcType
    val nullJdbcType = new NullJdbcType

    class BooleanJdbcType extends DriverJdbcType[Boolean] {
      def sqlType = java.sql.Types.BOOLEAN
      def setValue(v: Boolean, p: PositionedParameters) = p.setBoolean(v)
      def setOption(v: Option[Boolean], p: PositionedParameters) = p.setBooleanOption(v)
      def nextValue(r: PositionedResult) = r.nextBoolean
      def updateValue(v: Boolean, r: PositionedResult) = r.updateBoolean(v)
    }

    class BlobJdbcType extends DriverJdbcType[Blob] {
      def sqlType = java.sql.Types.BLOB
      def setValue(v: Blob, p: PositionedParameters) = p.setBlob(v)
      def setOption(v: Option[Blob], p: PositionedParameters) = p.setBlobOption(v)
      def nextValue(r: PositionedResult) = r.nextBlob
      def updateValue(v: Blob, r: PositionedResult) = r.updateBlob(v)
      override def valueToSQLLiteral(value: Blob) =
        throw new SlickException("Blob does not have a literal representation")
    }

    class ByteJdbcType extends DriverJdbcType[Byte] with NumericTypedType {
      def sqlType = java.sql.Types.TINYINT
      def setValue(v: Byte, p: PositionedParameters) = p.setByte(v)
      def setOption(v: Option[Byte], p: PositionedParameters) = p.setByteOption(v)
      def nextValue(r: PositionedResult) = r.nextByte
      def updateValue(v: Byte, r: PositionedResult) = r.updateByte(v)
    }

    class ByteArrayJdbcType extends DriverJdbcType[Array[Byte]] {
      def sqlType = java.sql.Types.BLOB
      def setValue(v: Array[Byte], p: PositionedParameters) = p.setBytes(v)
      def setOption(v: Option[Array[Byte]], p: PositionedParameters) = p.setBytesOption(v)
      def nextValue(r: PositionedResult) = r.nextBytes
      def updateValue(v: Array[Byte], r: PositionedResult) = r.updateBytes(v)
      override def valueToSQLLiteral(value: Array[Byte]) =
        throw new SlickException("Array[Byte] does not have a literal representation")
    }

    class ClobJdbcType extends DriverJdbcType[Clob] {
      def sqlType = java.sql.Types.CLOB
      def setValue(v: Clob, p: PositionedParameters) = p.setClob(v)
      def setOption(v: Option[Clob], p: PositionedParameters) = p.setClobOption(v)
      def nextValue(r: PositionedResult) = r.nextClob
      def updateValue(v: Clob, r: PositionedResult) = r.updateClob(v)
    }

    class CharJdbcType extends DriverJdbcType[Char] {
      def sqlType = java.sql.Types.CHAR
      override def sqlTypeName = "CHAR(1)"
      def setValue(v: Char, p: PositionedParameters) = stringJdbcType.setValue(String.valueOf(v), p)
      def setOption(v: Option[Char], p: PositionedParameters) = stringJdbcType.setOption(v.map(String.valueOf), p)
      def nextValue(r: PositionedResult) = {
        val s = stringJdbcType.nextValue(r)
        if(s.isEmpty) ' ' else s.charAt(0)
      }
      def updateValue(v: Char, r: PositionedResult) = stringJdbcType.updateValue(String.valueOf(v), r)
      override def valueToSQLLiteral(v: Char) = stringJdbcType.valueToSQLLiteral(String.valueOf(v))
    }

    class DateJdbcType extends DriverJdbcType[Date] {
      def sqlType = java.sql.Types.DATE
      def setValue(v: Date, p: PositionedParameters) = p.setDate(v)
      def setOption(v: Option[Date], p: PositionedParameters) = p.setDateOption(v)
      def nextValue(r: PositionedResult) = r.nextDate
      def updateValue(v: Date, r: PositionedResult) = r.updateDate(v)
      override def valueToSQLLiteral(value: Date) = "{d '"+value.toString+"'}"
    }

    class DoubleJdbcType extends DriverJdbcType[Double] with NumericTypedType {
      def sqlType = java.sql.Types.DOUBLE
      def setValue(v: Double, p: PositionedParameters) = p.setDouble(v)
      def setOption(v: Option[Double], p: PositionedParameters) = p.setDoubleOption(v)
      def nextValue(r: PositionedResult) = r.nextDouble
      def updateValue(v: Double, r: PositionedResult) = r.updateDouble(v)
    }

    class FloatJdbcType extends DriverJdbcType[Float] with NumericTypedType {
      def sqlType = java.sql.Types.FLOAT
      def setValue(v: Float, p: PositionedParameters) = p.setFloat(v)
      def setOption(v: Option[Float], p: PositionedParameters) = p.setFloatOption(v)
      def nextValue(r: PositionedResult) = r.nextFloat
      def updateValue(v: Float, r: PositionedResult) = r.updateFloat(v)
    }

    class IntJdbcType extends DriverJdbcType[Int] with NumericTypedType {
      def sqlType = java.sql.Types.INTEGER
      def setValue(v: Int, p: PositionedParameters) = p.setInt(v)
      def setOption(v: Option[Int], p: PositionedParameters) = p.setIntOption(v)
      def nextValue(r: PositionedResult) = r.nextInt
      def updateValue(v: Int, r: PositionedResult) = r.updateInt(v)
    }

    class LongJdbcType extends DriverJdbcType[Long] with NumericTypedType {
      def sqlType = java.sql.Types.BIGINT
      def setValue(v: Long, p: PositionedParameters) = p.setLong(v)
      def setOption(v: Option[Long], p: PositionedParameters) = p.setLongOption(v)
      def nextValue(r: PositionedResult) = r.nextLong
      def updateValue(v: Long, r: PositionedResult) = r.updateLong(v)
    }

    class ShortJdbcType extends DriverJdbcType[Short] with NumericTypedType {
      def sqlType = java.sql.Types.SMALLINT
      def setValue(v: Short, p: PositionedParameters) = p.setShort(v)
      def setOption(v: Option[Short], p: PositionedParameters) = p.setShortOption(v)
      def nextValue(r: PositionedResult) = r.nextShort
      def updateValue(v: Short, r: PositionedResult) = r.updateShort(v)
    }

    class StringJdbcType extends DriverJdbcType[String] {
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
      def sqlType = java.sql.Types.TIME
      def setValue(v: Time, p: PositionedParameters) = p.setTime(v)
      def setOption(v: Option[Time], p: PositionedParameters) = p.setTimeOption(v)
      def nextValue(r: PositionedResult) = r.nextTime
      def updateValue(v: Time, r: PositionedResult) = r.updateTime(v)
      override def valueToSQLLiteral(value: Time) = "{t '"+value.toString+"'}"
    }

    class TimestampJdbcType extends DriverJdbcType[Timestamp] {
      def sqlType = java.sql.Types.TIMESTAMP
      def setValue(v: Timestamp, p: PositionedParameters) = p.setTimestamp(v)
      def setOption(v: Option[Timestamp], p: PositionedParameters) = p.setTimestampOption(v)
      def nextValue(r: PositionedResult) = r.nextTimestamp
      def updateValue(v: Timestamp, r: PositionedResult) = r.updateTimestamp(v)
      override def valueToSQLLiteral(value: Timestamp) = "{ts '"+value.toString+"'}"
    }

    class UUIDJdbcType extends DriverJdbcType[UUID] {
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
      def sqlType = java.sql.Types.DECIMAL
      def setValue(v: BigDecimal, p: PositionedParameters) = p.setBigDecimal(v)
      def setOption(v: Option[BigDecimal], p: PositionedParameters) = p.setBigDecimalOption(v)
      def nextValue(r: PositionedResult) = r.nextBigDecimal
      def updateValue(v: BigDecimal, r: PositionedResult) = r.updateBigDecimal(v)
    }

    class NullJdbcType extends DriverJdbcType[Null] {
      def sqlType = java.sql.Types.NULL
      def setValue(v: Null, p: PositionedParameters) = p.setString(null)
      def setOption(v: Option[Null], p: PositionedParameters) = p.setString(null)
      def nextValue(r: PositionedResult) = { r.nextString; null }
      def updateValue(v: Null, r: PositionedResult) = r.updateNull()
      override def valueToSQLLiteral(value: Null) = "NULL"
    }
  }

  trait ImplicitColumnTypes extends super.ImplicitColumnTypes {
    implicit def booleanColumnType = columnTypes.booleanJdbcType
    implicit def blobColumnType = columnTypes.blobJdbcType
    implicit def byteColumnType = columnTypes.byteJdbcType
    implicit def byteArrayColumnType = columnTypes.byteArrayJdbcType
    implicit def charColumnType = columnTypes.charJdbcType
    implicit def clobColumnType = columnTypes.clobJdbcType
    implicit def dateColumnType = columnTypes.dateJdbcType
    implicit def doubleColumnType = columnTypes.doubleJdbcType
    implicit def floatColumnType = columnTypes.floatJdbcType
    implicit def intColumnType = columnTypes.intJdbcType
    implicit def longColumnType = columnTypes.longJdbcType
    implicit def shortColumnType = columnTypes.shortJdbcType
    implicit def stringColumnType = columnTypes.stringJdbcType
    implicit def timeColumnType = columnTypes.timeJdbcType
    implicit def timestampColumnType = columnTypes.timestampJdbcType
    implicit def uuidColumnType = columnTypes.uuidJdbcType
    implicit def bigDecimalColumnType = columnTypes.bigDecimalJdbcType
  }
}
