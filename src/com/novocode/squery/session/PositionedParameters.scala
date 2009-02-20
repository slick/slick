package com.novocode.squery.session

import java.sql.{PreparedStatement, Date, Time, Timestamp, Types}

class PositionedParameters(val ps: PreparedStatement) {

  var pos = 0

  def setBoolean(value: Boolean) = { pos += 1; ps.setBoolean(pos, value) }
  def setDate(value: Date) = { pos += 1; ps.setDate(pos, value) }
  def setDouble(value: Double) = { pos += 1; ps.setDouble(pos, value) }
  def setFloat(value: Float) = { pos += 1; ps.setFloat(pos, value) }
  def setInt(value: Int) = { pos += 1; ps.setInt(pos, value) }
  def setLong(value: Long) = { pos += 1; ps.setLong(pos, value) }
  def setString(value: String) = { pos += 1; ps.setString(pos, value) }
  def setTime(value: Time) = { pos += 1; ps.setTime(pos, value) }
  def setTimestamp(value: Timestamp) = { pos += 1; ps.setTimestamp(pos, value) }

  def setInt(value: java.lang.Integer) = { pos += 1; if(value eq null) ps.setNull(pos, Types.INTEGER) else ps.setInt(pos, value.intValue) }
  def setBoolean(value: java.lang.Boolean) = { pos += 1; if(value eq null) ps.setNull(pos, Types.BOOLEAN) else ps.setBoolean(pos, value.booleanValue) }

  def setBooleanOption(value: Option[Boolean]) = {
    pos += 1
    if(value eq None) ps.setNull(pos, Types.BOOLEAN) else ps.setBoolean(pos, value.get)
  }
  def setByteOption(value: Option[Byte]) = {
    pos += 1
    if(value eq None) ps.setNull(pos, Types.TINYINT) else ps.setByte(pos, value.get)
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
