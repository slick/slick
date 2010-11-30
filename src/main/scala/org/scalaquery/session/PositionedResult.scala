package org.scalaquery.session

import java.sql.{ResultSet, Blob, Clob, Date, Time, Timestamp}
import org.scalaquery.simple.GetResult

/**
 * A database result positioned at a row and column.
 */
abstract class PositionedResult(val rs: ResultSet) extends java.io.Closeable {
  private[this] lazy val meta = rs.getMetaData()

  var pos = 0
  lazy val numColumns = meta.getColumnCount()

  def hasMoreColumns = pos+1 < numColumns
  def next() = { pos = 0; rs.next }

  def << [T](implicit f: GetResult[T]): T = f(this)
  def <<? [T](implicit f: GetResult[Option[T]]): Option[T] = if(hasMoreColumns) this.<< else None

  def nextBoolean() = { pos += 1; rs getBoolean pos }
  def nextBlob() = { pos += 1; rs getBlob pos }
  def nextByte() = { pos += 1; rs getByte pos }
  def nextBytes() = { pos += 1; rs getBytes pos }
  def nextClob() = { pos += 1; rs getClob pos }
  def nextDate() = { pos += 1; rs getDate pos }
  def nextDouble() = { pos += 1; rs getDouble pos }
  def nextFloat() = { pos += 1; rs getFloat pos }
  def nextInt() = { pos += 1; rs getInt pos }
  def nextLong() = { pos += 1; rs getLong pos }
  def nextShort() = { pos += 1; rs getShort pos }
  def nextString() = { pos += 1; rs getString pos }
  def nextTime() = { pos += 1; rs getTime pos }
  def nextTimestamp() = { pos += 1; rs getTimestamp pos }

  def nextBooleanOption() = { pos += 1; val r = rs getBoolean pos; if(rs wasNull) None else Some(r) }
  def nextBlobOption() = { pos += 1; val r = rs getBlob pos; if(rs wasNull) None else Some(r) }
  def nextByteOption() = { pos += 1; val r = rs getByte pos; if(rs wasNull) None else Some(r) }
  def nextBytesOption() = { pos += 1; val r = rs getBytes pos; if(rs wasNull) None else Some(r) }
  def nextClobOption() = { pos += 1; val r = rs getClob pos; if(rs wasNull) None else Some(r) }
  def nextDateOption() = { pos += 1; val r = rs getDate pos; if(rs wasNull) None else Some(r) }
  def nextDoubleOption() = { pos += 1; val r = rs getDouble pos; if(rs wasNull) None else Some(r) }
  def nextFloatOption() = { pos += 1; val r = rs getFloat pos; if(rs wasNull) None else Some(r) }
  def nextIntOption() = { pos += 1; val r = rs getInt pos; if(rs wasNull) None else Some(r) }
  def nextLongOption() = { pos += 1; val r = rs getLong pos; if(rs wasNull) None else Some(r) }
  def nextShortOption() = { pos += 1; val r = rs getShort pos; if(rs wasNull) None else Some(r) }
  def nextStringOption() = { pos += 1; val r = rs getString pos; if(rs wasNull) None else Some(r) }
  def nextTimeOption() = { pos += 1; val r = rs getTime pos; if(rs wasNull) None else Some(r) }
  def nextTimestampOption() = { pos += 1; val r = rs getTimestamp pos; if(rs wasNull) None else Some(r) }

  def updateBoolean(v: Boolean) { pos += 1; rs.updateBoolean(pos, v) }
  def updateBlob(v: Blob) = { pos += 1; rs.updateBlob(pos, v) }
  def updateByte(v: Byte) = { pos += 1; rs.updateByte(pos, v) }
  def updateBytes(v: Array[Byte]) = { pos += 1; rs.updateBytes(pos, v) }
  def updateClob(v: Clob) = { pos += 1; rs.updateClob(pos, v) }
  def updateDate(v: Date) = { pos += 1; rs.updateDate(pos, v) }
  def updateDouble(v: Double) = { pos += 1; rs.updateDouble(pos, v) }
  def updateFloat(v: Float) = { pos += 1; rs.updateFloat(pos, v) }
  def updateInt(v: Int) = { pos += 1; rs.updateInt(pos, v) }
  def updateLong(v: Long) = { pos += 1; rs.updateLong(pos, v) }
  def updateShort(v: Short) = { pos += 1; rs.updateShort(pos, v) }
  def updateString(v: String) = { pos += 1; rs.updateString(pos, v) }
  def updateTime(v: Time) = { pos += 1; rs.updateTime(pos, v) }
  def updateTimestamp(v: Timestamp) = { pos += 1; rs.updateTimestamp(pos, v) }

  def updateBooleanOption(v: Option[Boolean]) { pos += 1; v match { case Some(s) => rs.updateBoolean(pos, s); case None => rs.updateNull(pos) } }
  def updateBlobOption(v: Option[Blob]) { pos += 1; v match { case Some(s) => rs.updateBlob(pos, s); case None => rs.updateNull(pos) } }
  def updateByteOption(v: Option[Byte]) { pos += 1; v match { case Some(s) => rs.updateByte(pos, s); case None => rs.updateNull(pos) } }
  def updateBytesOption(v: Option[Array[Byte]]) { pos += 1; v match { case Some(s) => rs.updateBytes(pos, s); case None => rs.updateNull(pos) } }
  def updateClobOption(v: Option[Clob]) { pos += 1; v match { case Some(s) => rs.updateClob(pos, s); case None => rs.updateNull(pos) } }
  def updateDateOption(v: Option[Date]) { pos += 1; v match { case Some(s) => rs.updateDate(pos, s); case None => rs.updateNull(pos) } }
  def updateDoubleOption(v: Option[Double]) { pos += 1; v match { case Some(s) => rs.updateDouble(pos, s); case None => rs.updateNull(pos) } }
  def updateFloatOption(v: Option[Float]) { pos += 1; v match { case Some(s) => rs.updateFloat(pos, s); case None => rs.updateNull(pos) } }
  def updateIntOption(v: Option[Int]) { pos += 1; v match { case Some(s) => rs.updateInt(pos, s); case None => rs.updateNull(pos) } }
  def updateLongOption(v: Option[Long]) { pos += 1; v match { case Some(s) => rs.updateLong(pos, s); case None => rs.updateNull(pos) } }
  def updateShortOption(v: Option[Short]) { pos += 1; v match { case Some(s) => rs.updateShort(pos, s); case None => rs.updateNull(pos) } }
  def updateStringOption(v: Option[String]) { pos += 1; v match { case Some(s) => rs.updateString(pos, s); case None => rs.updateNull(pos) } }
  def updateTimeOption(v: Option[Time]) { pos += 1; v match { case Some(s) => rs.updateTime(pos, s); case None => rs.updateNull(pos) } }
  def updateTimestampOption(v: Option[Timestamp]) { pos += 1; v match { case Some(s) => rs.updateTimestamp(pos, s); case None => rs.updateNull(pos) } }

  def skip = { pos += 1; this }
  def updateNull() { pos += 1; rs.updateNull(pos) }

  /**
   * Close the ResultSet and the statement which created it.
   */
  def close(): Unit
}
