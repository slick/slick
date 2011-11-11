package org.scalaquery.session

import java.sql.{PreparedStatement, Date, Time, Timestamp, Types, Blob, Clob}
import org.scalaquery.simple.SetParameter

class PositionedParameters(val ps: PreparedStatement) {

  var pos = 0

  def >> [T](value: T)(implicit f: SetParameter[T]): Unit = f(value, this)

  def setNull(sqlType: Int)            { val npos = pos + 1; ps.setNull(npos, sqlType);     pos = npos }

  def setBoolean(value: Boolean)       { val npos = pos + 1; ps.setBoolean   (npos, value); pos = npos }
  def setBlob(value: Blob)             { val npos = pos + 1; ps.setBlob      (npos, value); pos = npos }
  def setByte(value: Byte)             { val npos = pos + 1; ps.setByte      (npos, value); pos = npos }
  def setBytes(value: Array[Byte])     { val npos = pos + 1; ps.setBytes     (npos, value); pos = npos }
  def setClob(value: Clob)             { val npos = pos + 1; ps.setClob      (npos, value); pos = npos }
  def setDate(value: Date)             { val npos = pos + 1; ps.setDate      (npos, value); pos = npos }
  def setDouble(value: Double)         { val npos = pos + 1; ps.setDouble    (npos, value); pos = npos }
  def setFloat(value: Float)           { val npos = pos + 1; ps.setFloat     (npos, value); pos = npos }
  def setInt(value: Int)               { val npos = pos + 1; ps.setInt       (npos, value); pos = npos }
  def setLong(value: Long)             { val npos = pos + 1; ps.setLong      (npos, value); pos = npos }
  def setShort(value: Short)           { val npos = pos + 1; ps.setShort     (npos, value); pos = npos }
  def setString(value: String)         { val npos = pos + 1; ps.setString    (npos, value); pos = npos }
  def setTime(value: Time)             { val npos = pos + 1; ps.setTime      (npos, value); pos = npos }
  def setTimestamp(value: Timestamp)   { val npos = pos + 1; ps.setTimestamp (npos, value); pos = npos }
  def setBigDecimal(value: BigDecimal) { val npos = pos + 1; ps.setBigDecimal(npos, value.bigDecimal); pos = npos }
  def setObject(value: AnyRef, sqlType: Int) { val npos = pos + 1; ps.setObject(npos, value, sqlType); pos = npos }

  def setBooleanOption(value: Option[Boolean]) {
    val npos = pos + 1
    if(value eq None) ps.setNull(npos, Types.BOOLEAN) else ps.setBoolean(npos, value.get)
    pos = npos
  }
  def setBlobOption(value: Option[Blob]) {
    val npos = pos + 1
    if(value eq None) ps.setNull(npos, Types.BLOB) else ps.setBlob(npos, value.get)
    pos = npos
  }
  def setByteOption(value: Option[Byte]) {
    val npos = pos + 1
    if(value eq None) ps.setNull(npos, Types.TINYINT) else ps.setByte(npos, value.get)
    pos = npos
  }
  def setBytesOption(value: Option[Array[Byte]]) {
    val npos = pos + 1
    if(value eq None) ps.setNull(npos, Types.BLOB) else ps.setBytes(npos, value.get)
    pos = npos
  }
  def setClobOption(value: Option[Clob]) {
    val npos = pos + 1
    if(value eq None) ps.setNull(npos, Types.CLOB) else ps.setClob(npos, value.get)
    pos = npos
  }
  def setDateOption(value: Option[Date]) {
    val npos = pos + 1
    if(value eq None) ps.setNull(npos, Types.DATE) else ps.setDate(npos, value.get)
    pos = npos
  }
  def setDoubleOption(value: Option[Double]) {
    val npos = pos + 1
    if(value eq None) ps.setNull(npos, Types.DOUBLE) else ps.setDouble(npos, value.get)
    pos = npos
  }
  def setFloatOption(value: Option[Float]) {
    val npos = pos + 1
    if(value eq None) ps.setNull(npos, Types.FLOAT) else ps.setFloat(npos, value.get)
    pos = npos
  }
  def setIntOption(value: Option[Int]) {
    val npos = pos + 1
    if(value eq None) ps.setNull(npos, Types.INTEGER) else ps.setInt(npos, value.get)
    pos = npos
  }
  def setLongOption(value: Option[Long]) {
    val npos = pos + 1
    if(value eq None) ps.setNull(npos, Types.INTEGER) else ps.setLong(npos, value.get)
    pos = npos
  }
  def setShortOption(value: Option[Short]) {
    val npos = pos + 1
    if(value eq None) ps.setNull(npos, Types.SMALLINT) else ps.setShort(npos, value.get)
    pos = npos
  }
  def setStringOption(value: Option[String]) {
    val npos = pos + 1
    if(value eq None) ps.setNull(npos, Types.VARCHAR) else ps.setString(npos, value.get)
    pos = npos
  }
  def setTimeOption(value: Option[Time]) {
    val npos = pos + 1
    if(value eq None) ps.setNull(npos, Types.TIME) else ps.setTime(npos, value.get)
    pos = npos
  }
  def setTimestampOption(value: Option[Timestamp]) {
    val npos = pos + 1
    if(value eq None) ps.setNull(npos, Types.TIMESTAMP) else ps.setTimestamp(npos, value.get)
    pos = npos
  }
  def setBigDecimalOption(value: Option[BigDecimal]) {
    val npos = pos + 1
    if(value eq None) ps.setNull(npos, Types.DECIMAL) else ps.setBigDecimal(npos, value.get.bigDecimal)
    pos = npos
  }
  def setObjectOption(value: Option[AnyRef], sqlType: Int) {
    val npos = pos + 1
    if(value eq None) ps.setNull(npos, sqlType) else ps.setObject(npos, value.get, sqlType)
    pos = npos
  }
}
