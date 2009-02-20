package com.novocode.squery.session

import java.sql.ResultSet

class PositionedResult(val rs: ResultSet) {

  var pos = 0

  def next() = { pos = 0; rs.next }

  def nextBoolean() = { pos += 1; rs getBoolean pos }
  def nextByte() = { pos += 1; rs getByte pos }
  def nextDate() = { pos += 1; rs getDate pos }
  def nextDouble() = { pos += 1; rs getDouble pos }
  def nextFloat() = { pos += 1; rs getFloat pos }
  def nextInt() = { pos += 1; rs getInt pos }
  def nextLong() = { pos += 1; rs getLong pos }
  def nextShort() = { pos += 1; rs getShort pos }
  def nextString() = { pos += 1; rs getString pos }
  def nextTime() = { pos += 1; rs getTime pos }
  def nextTimestamp() = { pos += 1; rs getTimestamp pos }

  def nextBooleanOrNull() = { pos += 1; val r = rs getBoolean pos; if(rs wasNull) null else java.lang.Boolean.valueOf(r) }
  def nextIntegerOrNull() = { pos += 1; val r = rs getInt pos; if(rs wasNull) null else java.lang.Integer.valueOf(r) }

  def nextBooleanOption() = { pos += 1; val r = rs getBoolean pos; if(rs wasNull) None else Some(r) }
  def nextByteOption() = { pos += 1; val r = rs getByte pos; if(rs wasNull) None else Some(r) }
  def nextDateOption() = { pos += 1; val r = rs getDate pos; if(rs wasNull) None else Some(r) }
  def nextDoubleOption() = { pos += 1; val r = rs getDouble pos; if(rs wasNull) None else Some(r) }
  def nextFloatOption() = { pos += 1; val r = rs getFloat pos; if(rs wasNull) None else Some(r) }
  def nextIntOption() = { pos += 1; val r = rs getInt pos; if(rs wasNull) None else Some(r) }
  def nextLongOption() = { pos += 1; val r = rs getLong pos; if(rs wasNull) None else Some(r) }
  def nextShortOption() = { pos += 1; val r = rs getShort pos; if(rs wasNull) None else Some(r) }
  def nextStringOption() = { pos += 1; val r = rs getString pos; if(rs wasNull) None else Some(r) }
  def nextTimeOption() = { pos += 1; val r = rs getTime pos; if(rs wasNull) None else Some(r) }
  def nextTimestampOption() = { pos += 1; val r = rs getTimestamp pos; if(rs wasNull) None else Some(r) }
}
