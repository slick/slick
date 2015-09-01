package slick.driver

import java.sql.{Blob, Clob, Date, Time, Timestamp, ResultSet, PreparedStatement}
import java.util.UUID
import slick.SlickException
import slick.ast._
import slick.jdbc.JdbcType
import slick.profile.{RelationalProfile, RelationalTypesComponent}
import scala.reflect.ClassTag

trait JdbcTypesComponent extends RelationalTypesComponent { driver: JdbcDriver =>

  abstract class MappedJdbcType[T, U](implicit val tmd: JdbcType[U], val classTag: ClassTag[T]) extends JdbcType[T] {
    def map(t: T): U
    def comap(u: U): T

    def newSqlType: Option[Int] = None
    def newSqlTypeName(size: Option[FieldSymbol]): Option[String] = None
    def newValueToSQLLiteral(value: T): Option[String] = None
    def newHasLiteralForm: Option[Boolean] = None

    def sqlType = newSqlType.getOrElse(tmd.sqlType)
    def sqlTypeName(sym: Option[FieldSymbol]) = newSqlTypeName(sym).getOrElse(tmd.sqlTypeName(sym))
    def setValue(v: T, p: PreparedStatement, idx: Int) = tmd.setValue(map(v), p, idx)
    def setNull(p: PreparedStatement, idx: Int): Unit = tmd.setNull(p, idx)
    def getValue(r: ResultSet, idx: Int) = {
      val v = tmd.getValue(r, idx)
      if((v.asInstanceOf[AnyRef] eq null) || tmd.wasNull(r, idx)) null.asInstanceOf[T]
      else comap(v)
    }
    def wasNull(r: ResultSet, idx: Int) = tmd.wasNull(r, idx)
    def updateValue(v: T, r: ResultSet, idx: Int) = tmd.updateValue(map(v), r, idx)
    def valueToSQLLiteral(value: T) = newValueToSQLLiteral(value).getOrElse(tmd.valueToSQLLiteral(map(value)))
    def hasLiteralForm = newHasLiteralForm.getOrElse(tmd.hasLiteralForm)
    def scalaType = ScalaBaseType[T]
    override def toString = s"MappedJdbcType[${classTag.runtimeClass.getName} -> $tmd]"
    override def hashCode = tmd.hashCode() + classTag.hashCode()
    override def equals(o: Any) = o match {
      case o: MappedJdbcType[_, _] => tmd == o.tmd && classTag == o.classTag
      case _ => false
    }
  }

  object MappedJdbcType extends MappedColumnTypeFactory {
    def base[T : ClassTag, U : BaseColumnType](tmap: T => U, tcomap: U => T): BaseColumnType[T] = {
      assertNonNullType(implicitly[BaseColumnType[U]])
      new MappedJdbcType[T, U] with BaseTypedType[T] {
        def map(t: T) = tmap(t)
        def comap(u: U) = tcomap(u)
      }
    }
  }

  object JdbcType {
    def unapply(t: Type) = Some((jdbcTypeFor(t), t.isInstanceOf[OptionType]))
  }

  def jdbcTypeFor(t: Type): JdbcType[Any] = ((t.structural match {
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
    case t: OptionType => jdbcTypeFor(t.elementType)
    case t: ErasedScalaBaseType[_, _] => jdbcTypeFor(t.erasure)
    case t => throw new SlickException("JdbcProfile has no JdbcType for type "+t)
  }): JdbcType[_]).asInstanceOf[JdbcType[Any]]

  def defaultSqlTypeName(tmd: JdbcType[_], sym: Option[FieldSymbol]): String = tmd.sqlType match {
    case java.sql.Types.VARCHAR =>
      val size = sym.flatMap(_.findColumnOption[RelationalProfile.ColumnOption.Length])
      size.fold("VARCHAR(254)")(l => if(l.varying) s"VARCHAR(${l.length})" else s"CHAR(${l.length})")
    case java.sql.Types.DECIMAL => "DECIMAL(21,2)"
    case t => JdbcTypesComponent.typeNames.getOrElse(t,
      throw new SlickException("No SQL type name found in java.sql.Types for code "+t))
  }

  abstract class DriverJdbcType[@specialized T](implicit val classTag: ClassTag[T]) extends JdbcType[T] {
    def scalaType = ScalaBaseType[T]
    def sqlTypeName(sym: Option[FieldSymbol]): String = driver.defaultSqlTypeName(this, sym)
    def valueToSQLLiteral(value: T) =
      if(hasLiteralForm) value.toString
      else throw new SlickException(sqlTypeName(None) + " does not have a literal representation")
    def hasLiteralForm = true
    def wasNull(r: ResultSet, idx: Int) = r.wasNull()
    def setNull(p: PreparedStatement, idx: Int): Unit = p.setNull(idx, sqlType)
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
      def setValue(v: Boolean, p: PreparedStatement, idx: Int) = p.setBoolean(idx, v)
      def getValue(r: ResultSet, idx: Int) = r.getBoolean(idx)
      def updateValue(v: Boolean, r: ResultSet, idx: Int) = r.updateBoolean(idx, v)
    }

    class BlobJdbcType extends DriverJdbcType[Blob] {
      def sqlType = java.sql.Types.BLOB
      def setValue(v: Blob, p: PreparedStatement, idx: Int) = p.setBlob(idx, v)
      def getValue(r: ResultSet, idx: Int) = r.getBlob(idx)
      def updateValue(v: Blob, r: ResultSet, idx: Int) = r.updateBlob(idx, v)
      override def hasLiteralForm = false
    }

    class ByteJdbcType extends DriverJdbcType[Byte] with NumericTypedType {
      def sqlType = java.sql.Types.TINYINT
      def setValue(v: Byte, p: PreparedStatement, idx: Int) = p.setByte(idx, v)
      def getValue(r: ResultSet, idx: Int) = r.getByte(idx)
      def updateValue(v: Byte, r: ResultSet, idx: Int) = r.updateByte(idx, v)
    }

    class ByteArrayJdbcType extends DriverJdbcType[Array[Byte]] {
      def sqlType = java.sql.Types.BLOB
      def setValue(v: Array[Byte], p: PreparedStatement, idx: Int) = p.setBytes(idx, v)
      def getValue(r: ResultSet, idx: Int) = r.getBytes(idx)
      def updateValue(v: Array[Byte], r: ResultSet, idx: Int) = r.updateBytes(idx, v)
      override def hasLiteralForm = false
    }

    class ClobJdbcType extends DriverJdbcType[Clob] {
      def sqlType = java.sql.Types.CLOB
      def setValue(v: Clob, p: PreparedStatement, idx: Int) = p.setClob(idx, v)
      def getValue(r: ResultSet, idx: Int) = r.getClob(idx)
      def updateValue(v: Clob, r: ResultSet, idx: Int) = r.updateClob(idx, v)
      override def hasLiteralForm = false
    }

    class CharJdbcType extends DriverJdbcType[Char] {
      def sqlType = java.sql.Types.CHAR
      override def sqlTypeName(sym: Option[FieldSymbol]) = "CHAR(1)"
      def setValue(v: Char, p: PreparedStatement, idx: Int) = stringJdbcType.setValue(String.valueOf(v), p, idx)
      def getValue(r: ResultSet, idx: Int) = {
        val s = stringJdbcType.getValue(r, idx)
        if(s == null || s.isEmpty) ' ' else s.charAt(0)
      }
      def updateValue(v: Char, r: ResultSet, idx: Int) = stringJdbcType.updateValue(String.valueOf(v), r, idx)
      override def valueToSQLLiteral(v: Char) = stringJdbcType.valueToSQLLiteral(String.valueOf(v))
    }

    class DateJdbcType extends DriverJdbcType[Date] {
      def sqlType = java.sql.Types.DATE
      def setValue(v: Date, p: PreparedStatement, idx: Int) = p.setDate(idx, v)
      def getValue(r: ResultSet, idx: Int) = r.getDate(idx)
      def updateValue(v: Date, r: ResultSet, idx: Int) = r.updateDate(idx, v)
      override def valueToSQLLiteral(value: Date) = "{d '"+value.toString+"'}"
    }

    class DoubleJdbcType extends DriverJdbcType[Double] with NumericTypedType {
      def sqlType = java.sql.Types.DOUBLE
      def setValue(v: Double, p: PreparedStatement, idx: Int) = p.setDouble(idx, v)
      def getValue(r: ResultSet, idx: Int) = r.getDouble(idx)
      def updateValue(v: Double, r: ResultSet, idx: Int) = r.updateDouble(idx, v)
    }

    class FloatJdbcType extends DriverJdbcType[Float] with NumericTypedType {
      def sqlType = java.sql.Types.REAL // see http://docs.oracle.com/javase/1.5.0/docs/guide/jdbc/getstart/mapping.html#1055162
      def setValue(v: Float, p: PreparedStatement, idx: Int) = p.setFloat(idx, v)
      def getValue(r: ResultSet, idx: Int) = r.getFloat(idx)
      def updateValue(v: Float, r: ResultSet, idx: Int) = r.updateFloat(idx, v)
    }

    class IntJdbcType extends DriverJdbcType[Int] with NumericTypedType {
      def sqlType = java.sql.Types.INTEGER
      def setValue(v: Int, p: PreparedStatement, idx: Int) = p.setInt(idx, v)
      def getValue(r: ResultSet, idx: Int) = r.getInt(idx)
      def updateValue(v: Int, r: ResultSet, idx: Int) = r.updateInt(idx, v)
    }

    class LongJdbcType extends DriverJdbcType[Long] with NumericTypedType {
      def sqlType = java.sql.Types.BIGINT
      def setValue(v: Long, p: PreparedStatement, idx: Int) = p.setLong(idx, v)
      def getValue(r: ResultSet, idx: Int) = r.getLong(idx)
      def updateValue(v: Long, r: ResultSet, idx: Int) = r.updateLong(idx, v)
    }

    class ShortJdbcType extends DriverJdbcType[Short] with NumericTypedType {
      def sqlType = java.sql.Types.SMALLINT
      def setValue(v: Short, p: PreparedStatement, idx: Int) = p.setShort(idx, v)
      def getValue(r: ResultSet, idx: Int) = r.getShort(idx)
      def updateValue(v: Short, r: ResultSet, idx: Int) = r.updateShort(idx, v)
    }

    class StringJdbcType extends DriverJdbcType[String] {
      def sqlType = java.sql.Types.VARCHAR
      def setValue(v: String, p: PreparedStatement, idx: Int) = p.setString(idx, v)
      def getValue(r: ResultSet, idx: Int) = r.getString(idx)
      def updateValue(v: String, r: ResultSet, idx: Int) = r.updateString(idx, v)
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
      def setValue(v: Time, p: PreparedStatement, idx: Int) = p.setTime(idx, v)
      def getValue(r: ResultSet, idx: Int) = r.getTime(idx)
      def updateValue(v: Time, r: ResultSet, idx: Int) = r.updateTime(idx, v)
      override def valueToSQLLiteral(value: Time) = "{t '"+value.toString+"'}"
    }

    class TimestampJdbcType extends DriverJdbcType[Timestamp] {
      def sqlType = java.sql.Types.TIMESTAMP
      def setValue(v: Timestamp, p: PreparedStatement, idx: Int) = p.setTimestamp(idx, v)
      def getValue(r: ResultSet, idx: Int) = r.getTimestamp(idx)
      def updateValue(v: Timestamp, r: ResultSet, idx: Int) = r.updateTimestamp(idx, v)
      override def valueToSQLLiteral(value: Timestamp) = "{ts '"+value.toString+"'}"
    }

    class UUIDJdbcType extends DriverJdbcType[UUID] {
      def sqlType = java.sql.Types.OTHER
      def setValue(v: UUID, p: PreparedStatement, idx: Int) = p.setBytes(idx, toBytes(v))
      def getValue(r: ResultSet, idx: Int) = fromBytes(r.getBytes(idx))
      def updateValue(v: UUID, r: ResultSet, idx: Int) = r.updateBytes(idx, toBytes(v))
      override def hasLiteralForm = false
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
      def setValue(v: BigDecimal, p: PreparedStatement, idx: Int) = p.setBigDecimal(idx, v.bigDecimal)
      def getValue(r: ResultSet, idx: Int) = {
        val v = r.getBigDecimal(idx)
        if(v eq null) null else BigDecimal(v)
      }
      def updateValue(v: BigDecimal, r: ResultSet, idx: Int) = r.updateBigDecimal(idx, v.bigDecimal)
    }

    class NullJdbcType extends DriverJdbcType[Null] {
      def sqlType = java.sql.Types.NULL
      def setValue(v: Null, p: PreparedStatement, idx: Int) = p.setString(idx, null)
      override def setNull(p: PreparedStatement, idx: Int) = p.setString(idx, null)
      def getValue(r: ResultSet, idx: Int) = null
      def updateValue(v: Null, r: ResultSet, idx: Int) = r.updateNull(idx)
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

object JdbcTypesComponent {
  private[slick] lazy val typeNames = Map() ++
    (for(f <- classOf[java.sql.Types].getFields)
    yield f.get(null).asInstanceOf[Int] -> f.getName)
}
