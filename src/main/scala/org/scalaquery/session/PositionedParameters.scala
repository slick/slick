package org.scalaquery.session

import java.sql.{PreparedStatement, Date, Time, Timestamp, Types, Blob, Clob}
import org.scalaquery.simple.SetParameter

class PositionedParameters(val ps: PreparedStatement) {

  var pos = 0

  def >> [T](value: T)(implicit f: SetParameter[T]): Unit = f(value, this)

  def setBoolean(value: Boolean) = { pos += 1; ps.setBoolean(pos, value) }
  def setBlob(value: Blob) = { pos += 1; ps.setBlob(pos, value) }
  def setByte(value: Byte) = { pos += 1; ps.setByte(pos, value) }
  def setBytes(value: Array[Byte]) = { pos += 1; ps.setBytes(pos, value) }
  def setClob(value: Clob) = { pos += 1; ps.setClob(pos, value) }
  def setDate(value: Date) = { pos += 1; ps.setDate(pos, value) }
  def setDouble(value: Double) = { pos += 1; ps.setDouble(pos, value) }
  def setFloat(value: Float) = { pos += 1; ps.setFloat(pos, value) }
  def setInt(value: Int) = { pos += 1; ps.setInt(pos, value) }
  def setLong(value: Long) = { pos += 1; ps.setLong(pos, value) }
  def setShort(value: Short) = { pos += 1; ps.setShort(pos, value) }
  def setString(value: String) = { pos += 1; ps.setString(pos, value) }
  def setTime(value: Time) = { pos += 1; ps.setTime(pos, value) }
  def setTimestamp(value: Timestamp) = { pos += 1; ps.setTimestamp(pos, value) }

  def setBooleanOption(value: Option[Boolean]) = {
    pos += 1
    if(value eq None) ps.setNull(pos, Types.BOOLEAN) else ps.setBoolean(pos, value.get)
  }
  def setBlobOption(value: Option[Blob]) = {
    pos += 1
    if(value eq None) ps.setNull(pos, Types.BLOB) else ps.setBlob(pos, value.get)
  }
  def setByteOption(value: Option[Byte]) = {
    pos += 1
    if(value eq None) ps.setNull(pos, Types.TINYINT) else ps.setByte(pos, value.get)
  }
  def setBytesOption(value: Option[Array[Byte]]) = {
    pos += 1
    if(value eq None) ps.setNull(pos, Types.BLOB) else ps.setBytes(pos, value.get)
  }
  def setClobOption(value: Option[Clob]) = {
    pos += 1
    if(value eq None) ps.setNull(pos, Types.CLOB) else ps.setClob(pos, value.get)
  }
  def setDateOption(value: Option[Date]) = {
    pos += 1
    if(value eq None) ps.setNull(pos, Types.DATE) else ps.setDate(pos, value.get)
  }
  def setDoubleOption(value: Option[Double]) = {
    pos += 1
    if(value eq None) ps.setNull(pos, Types.DOUBLE) else ps.setDouble(pos, value.get)
  }
  def setFloatOption(value: Option[Float]) = {
    pos += 1
    if(value eq None) ps.setNull(pos, Types.FLOAT) else ps.setFloat(pos, value.get)
  }
  def setIntOption(value: Option[Int]) = {
    pos += 1
    if(value eq None) ps.setNull(pos, Types.INTEGER) else ps.setInt(pos, value.get)
  }
  def setLongOption(value: Option[Long]) = {
    pos += 1
    if(value eq None) ps.setNull(pos, Types.INTEGER) else ps.setLong(pos, value.get)
  }
  def setShortOption(value: Option[Short]) = {
    pos += 1
    if(value eq None) ps.setNull(pos, Types.SMALLINT) else ps.setShort(pos, value.get)
  }
  def setStringOption(value: Option[String]) = {
    pos += 1
    if(value eq None) ps.setNull(pos, Types.VARCHAR) else ps.setString(pos, value.get)
  }
  def setTimeOption(value: Option[Time]) = {
    pos += 1
    if(value eq None) ps.setNull(pos, Types.TIME) else ps.setTime(pos, value.get)
  }
  def setTimestampOption(value: Option[Timestamp]) = {
    pos += 1
    if(value eq None) ps.setNull(pos, Types.TIMESTAMP) else ps.setTimestamp(pos, value.get)
  }
}
