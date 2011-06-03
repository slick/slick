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

  def hasMoreColumns = pos < numColumns
  def next() = { val ret = rs.next; pos = 0; ret }

  def << [T](implicit f: GetResult[T]): T = f(this)
  def <<? [T](implicit f: GetResult[Option[T]]): Option[T] = if(hasMoreColumns) this.<< else None

  def nextBoolean()   = { val npos = pos + 1; val r = rs getBoolean   npos; pos = npos; r }
  def nextBlob()      = { val npos = pos + 1; val r = rs getBlob      npos; pos = npos; r }
  def nextByte()      = { val npos = pos + 1; val r = rs getByte      npos; pos = npos; r }
  def nextBytes()     = { val npos = pos + 1; val r = rs getBytes     npos; pos = npos; r }
  def nextClob()      = { val npos = pos + 1; val r = rs getClob      npos; pos = npos; r }
  def nextDate()      = { val npos = pos + 1; val r = rs getDate      npos; pos = npos; r }
  def nextDouble()    = { val npos = pos + 1; val r = rs getDouble    npos; pos = npos; r }
  def nextFloat()     = { val npos = pos + 1; val r = rs getFloat     npos; pos = npos; r }
  def nextInt()       = { val npos = pos + 1; val r = rs getInt       npos; pos = npos; r }
  def nextLong()      = { val npos = pos + 1; val r = rs getLong      npos; pos = npos; r }
  def nextShort()     = { val npos = pos + 1; val r = rs getShort     npos; pos = npos; r }
  def nextString()    = { val npos = pos + 1; val r = rs getString    npos; pos = npos; r }
  def nextTime()      = { val npos = pos + 1; val r = rs getTime      npos; pos = npos; r }
  def nextTimestamp() = { val npos = pos + 1; val r = rs getTimestamp npos; pos = npos; r }

  def nextBooleanOption()   = { val npos = pos + 1; val r = rs getBoolean   npos; val rr = (if(rs wasNull) None else Some(r)); pos = npos; rr }
  def nextBlobOption()      = { val npos = pos + 1; val r = rs getBlob      npos; val rr = (if(rs wasNull) None else Some(r)); pos = npos; rr }
  def nextByteOption()      = { val npos = pos + 1; val r = rs getByte      npos; val rr = (if(rs wasNull) None else Some(r)); pos = npos; rr }
  def nextBytesOption()     = { val npos = pos + 1; val r = rs getBytes     npos; val rr = (if(rs wasNull) None else Some(r)); pos = npos; rr }
  def nextClobOption()      = { val npos = pos + 1; val r = rs getClob      npos; val rr = (if(rs wasNull) None else Some(r)); pos = npos; rr }
  def nextDateOption()      = { val npos = pos + 1; val r = rs getDate      npos; val rr = (if(rs wasNull) None else Some(r)); pos = npos; rr }
  def nextDoubleOption()    = { val npos = pos + 1; val r = rs getDouble    npos; val rr = (if(rs wasNull) None else Some(r)); pos = npos; rr }
  def nextFloatOption()     = { val npos = pos + 1; val r = rs getFloat     npos; val rr = (if(rs wasNull) None else Some(r)); pos = npos; rr }
  def nextIntOption()       = { val npos = pos + 1; val r = rs getInt       npos; val rr = (if(rs wasNull) None else Some(r)); pos = npos; rr }
  def nextLongOption()      = { val npos = pos + 1; val r = rs getLong      npos; val rr = (if(rs wasNull) None else Some(r)); pos = npos; rr }
  def nextShortOption()     = { val npos = pos + 1; val r = rs getShort     npos; val rr = (if(rs wasNull) None else Some(r)); pos = npos; rr }
  def nextStringOption()    = { val npos = pos + 1; val r = rs getString    npos; val rr = (if(rs wasNull) None else Some(r)); pos = npos; rr }
  def nextTimeOption()      = { val npos = pos + 1; val r = rs getTime      npos; val rr = (if(rs wasNull) None else Some(r)); pos = npos; rr }
  def nextTimestampOption() = { val npos = pos + 1; val r = rs getTimestamp npos; val rr = (if(rs wasNull) None else Some(r)); pos = npos; rr }

  def updateBoolean(v: Boolean)     { val npos = pos + 1; rs.updateBoolean  (npos, v); pos = npos }
  def updateBlob(v: Blob)           { val npos = pos + 1; rs.updateBlob     (npos, v); pos = npos }
  def updateByte(v: Byte)           { val npos = pos + 1; rs.updateByte     (npos, v); pos = npos }
  def updateBytes(v: Array[Byte])   { val npos = pos + 1; rs.updateBytes    (npos, v); pos = npos }
  def updateClob(v: Clob)           { val npos = pos + 1; rs.updateClob     (npos, v); pos = npos }
  def updateDate(v: Date)           { val npos = pos + 1; rs.updateDate     (npos, v); pos = npos }
  def updateDouble(v: Double)       { val npos = pos + 1; rs.updateDouble   (npos, v); pos = npos }
  def updateFloat(v: Float)         { val npos = pos + 1; rs.updateFloat    (npos, v); pos = npos }
  def updateInt(v: Int)             { val npos = pos + 1; rs.updateInt      (npos, v); pos = npos }
  def updateLong(v: Long)           { val npos = pos + 1; rs.updateLong     (npos, v); pos = npos }
  def updateShort(v: Short)         { val npos = pos + 1; rs.updateShort    (npos, v); pos = npos }
  def updateString(v: String)       { val npos = pos + 1; rs.updateString   (npos, v); pos = npos }
  def updateTime(v: Time)           { val npos = pos + 1; rs.updateTime     (npos, v); pos = npos }
  def updateTimestamp(v: Timestamp) { val npos = pos + 1; rs.updateTimestamp(npos, v); pos = npos }

  def updateBooleanOption(v: Option[Boolean])     { val npos = pos + 1; v match { case Some(s) => rs.updateBoolean  (npos, s); case None => rs.updateNull(npos) }; pos = npos }
  def updateBlobOption(v: Option[Blob])           { val npos = pos + 1; v match { case Some(s) => rs.updateBlob     (npos, s); case None => rs.updateNull(npos) }; pos = npos }
  def updateByteOption(v: Option[Byte])           { val npos = pos + 1; v match { case Some(s) => rs.updateByte     (npos, s); case None => rs.updateNull(npos) }; pos = npos }
  def updateBytesOption(v: Option[Array[Byte]])   { val npos = pos + 1; v match { case Some(s) => rs.updateBytes    (npos, s); case None => rs.updateNull(npos) }; pos = npos }
  def updateClobOption(v: Option[Clob])           { val npos = pos + 1; v match { case Some(s) => rs.updateClob     (npos, s); case None => rs.updateNull(npos) }; pos = npos }
  def updateDateOption(v: Option[Date])           { val npos = pos + 1; v match { case Some(s) => rs.updateDate     (npos, s); case None => rs.updateNull(npos) }; pos = npos }
  def updateDoubleOption(v: Option[Double])       { val npos = pos + 1; v match { case Some(s) => rs.updateDouble   (npos, s); case None => rs.updateNull(npos) }; pos = npos }
  def updateFloatOption(v: Option[Float])         { val npos = pos + 1; v match { case Some(s) => rs.updateFloat    (npos, s); case None => rs.updateNull(npos) }; pos = npos }
  def updateIntOption(v: Option[Int])             { val npos = pos + 1; v match { case Some(s) => rs.updateInt      (npos, s); case None => rs.updateNull(npos) }; pos = npos }
  def updateLongOption(v: Option[Long])           { val npos = pos + 1; v match { case Some(s) => rs.updateLong     (npos, s); case None => rs.updateNull(npos) }; pos = npos }
  def updateShortOption(v: Option[Short])         { val npos = pos + 1; v match { case Some(s) => rs.updateShort    (npos, s); case None => rs.updateNull(npos) }; pos = npos }
  def updateStringOption(v: Option[String])       { val npos = pos + 1; v match { case Some(s) => rs.updateString   (npos, s); case None => rs.updateNull(npos) }; pos = npos }
  def updateTimeOption(v: Option[Time])           { val npos = pos + 1; v match { case Some(s) => rs.updateTime     (npos, s); case None => rs.updateNull(npos) }; pos = npos }
  def updateTimestampOption(v: Option[Timestamp]) { val npos = pos + 1; v match { case Some(s) => rs.updateTimestamp(npos, s); case None => rs.updateNull(npos) }; pos = npos }

  def skip = { pos += 1; this }
  def updateNull() { val npos = pos + 1; rs.updateNull(npos); pos = npos }

  /**
   * Close the ResultSet and the statement which created it.
   */
  def close(): Unit
}
