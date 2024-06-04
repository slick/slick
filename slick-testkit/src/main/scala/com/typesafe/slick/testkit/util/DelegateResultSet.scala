package com.typesafe.slick.testkit.util

import java.io._
import java.sql.{Array => SQLArray, _}
import java.util.{Map, Calendar}
import java.math.BigDecimal
import java.net.URL

/**
 * An implementation of ResultSet which delegates all calls to another ResultSet. Individual methods can be overridden
 * in subclasses to implement custom behavior. Methods from JDK 7 are declared but they throw a NotImplementedException
 * instead of delegating the call, so that this class can be compiled on both, JDK 6 and JDK 7.
 */
class DelegateResultSet(rs: ResultSet) extends ResultSet {
  def next(): Boolean = rs.next()
  def isWrapperFor(iface: Class[_]): Boolean = rs.isWrapperFor(iface)
  def unwrap[T](iface: Class[T]): T = rs.unwrap(iface)
  def getObject[T](columnLabel: String, `type`: Class[T]): T = ??? // rs.getObject[T](columnLabel, `type`)
  def getObject[T](columnIndex: Int, `type`: Class[T]): T = ??? // rs.getObject[T](columnIndex, `type`)
  def updateNClob(columnLabel: String, reader: Reader): Unit = rs.updateNClob(columnLabel, reader)
  def updateNClob(columnIndex: Int, reader: Reader): Unit = rs.updateNClob(columnIndex, reader)
  def updateClob(columnLabel: String, reader: Reader): Unit = rs.updateClob(columnLabel, reader)
  def updateClob(columnIndex: Int, reader: Reader): Unit = rs.updateClob(columnIndex, reader)
  def updateBlob(columnLabel: String, inputStream: InputStream): Unit = rs.updateBlob(columnLabel, inputStream)
  def updateBlob(columnIndex: Int, inputStream: InputStream): Unit = rs.updateBlob(columnIndex, inputStream)
  def updateCharacterStream(columnLabel: String, reader: Reader): Unit = rs.updateCharacterStream(columnLabel, reader)
  def updateBinaryStream(columnLabel: String, x: InputStream): Unit = rs.updateBinaryStream(columnLabel, x)
  def updateAsciiStream(columnLabel: String, x: InputStream): Unit = rs.updateAsciiStream(columnLabel, x)
  def updateCharacterStream(columnIndex: Int, x: Reader): Unit = rs.updateCharacterStream(columnIndex, x)
  def updateBinaryStream(columnIndex: Int, x: InputStream): Unit = rs.updateBinaryStream(columnIndex, x)
  def updateAsciiStream(columnIndex: Int, x: InputStream): Unit = rs.updateAsciiStream(columnIndex, x)
  def updateNCharacterStream(columnLabel: String, reader: Reader): Unit = rs.updateNCharacterStream(columnLabel, reader)
  def updateNCharacterStream(columnIndex: Int, x: Reader): Unit = rs.updateNCharacterStream(columnIndex, x)
  def updateNClob(columnLabel: String, reader: Reader, length: Long): Unit = rs.updateNClob(columnLabel, reader, length)
  def updateNClob(columnIndex: Int, reader: Reader, length: Long): Unit = rs.updateNClob(columnIndex, reader, length)
  def updateClob(columnLabel: String, reader: Reader, length: Long): Unit = rs.updateClob(columnLabel, reader, length)
  def updateClob(columnIndex: Int, reader: Reader, length: Long): Unit = rs.updateClob(columnIndex, reader, length)
  def updateBlob(columnLabel: String, inputStream: InputStream, length: Long): Unit = rs
    .updateBlob(columnLabel, inputStream, length)
  def updateBlob(columnIndex: Int, inputStream: InputStream, length: Long): Unit = rs
    .updateBlob(columnIndex, inputStream, length)
  def updateCharacterStream(columnLabel: String, reader: Reader, length: Long): Unit = rs
    .updateCharacterStream(columnLabel, reader, length)
  def updateBinaryStream(columnLabel: String, x: InputStream, length: Long): Unit = rs
    .updateBinaryStream(columnLabel, x, length)
  def updateAsciiStream(columnLabel: String, x: InputStream, length: Long): Unit = rs
    .updateAsciiStream(columnLabel, x, length)
  def updateCharacterStream(columnIndex: Int, x: Reader, length: Long): Unit = rs
    .updateCharacterStream(columnIndex, x, length)
  def updateBinaryStream(columnIndex: Int, x: InputStream, length: Long): Unit = rs
    .updateBinaryStream(columnIndex, x, length)
  def updateAsciiStream(columnIndex: Int, x: InputStream, length: Long): Unit = rs
    .updateAsciiStream(columnIndex, x, length)
  def updateNCharacterStream(columnLabel: String, reader: Reader, length: Long): Unit = rs
    .updateNCharacterStream(columnLabel, reader, length)
  def updateNCharacterStream(columnIndex: Int, x: Reader, length: Long): Unit = rs
    .updateNCharacterStream(columnIndex, x, length)
  def getNCharacterStream(columnLabel: String): Reader = rs.getNCharacterStream(columnLabel)
  def getNCharacterStream(columnIndex: Int): Reader = rs.getNCharacterStream(columnIndex)
  def getNString(columnLabel: String): String = rs.getNString(columnLabel)
  def getNString(columnIndex: Int): String = rs.getNString(columnIndex)
  def updateSQLXML(columnLabel: String, xmlObject: SQLXML): Unit = rs.updateSQLXML(columnLabel, xmlObject)
  def updateSQLXML(columnIndex: Int, xmlObject: SQLXML): Unit = rs.updateSQLXML(columnIndex, xmlObject)
  def getSQLXML(columnLabel: String): SQLXML = rs.getSQLXML(columnLabel)
  def getSQLXML(columnIndex: Int): SQLXML = rs.getSQLXML(columnIndex)
  def getNClob(columnLabel: String): NClob = rs.getNClob(columnLabel)
  def getNClob(columnIndex: Int): NClob = rs.getNClob(columnIndex)
  def updateNClob(columnLabel: String, nClob: NClob): Unit = rs.updateNClob(columnLabel, nClob)
  def updateNClob(columnIndex: Int, nClob: NClob): Unit = rs.updateNClob(columnIndex, nClob)
  def updateNString(columnLabel: String, nString: String): Unit = rs.updateNString(columnLabel, nString)
  def updateNString(columnIndex: Int, nString: String): Unit = rs.updateNString(columnIndex, nString)
  def isClosed: Boolean = rs.isClosed
  def getHoldability: Int = rs.getHoldability
  def updateRowId(columnLabel: String, x: RowId): Unit = rs.updateRowId(columnLabel, x)
  def updateRowId(columnIndex: Int, x: RowId): Unit = rs.updateRowId(columnIndex, x)
  def getRowId(columnLabel: String): RowId = rs.getRowId(columnLabel)
  def getRowId(columnIndex: Int): RowId = rs.getRowId(columnIndex)
  def updateArray(columnLabel: String, x: SQLArray): Unit = rs.updateArray(columnLabel, x)
  def updateArray(columnIndex: Int, x: SQLArray): Unit = rs.updateArray(columnIndex, x)
  def updateClob(columnLabel: String, x: Clob): Unit = rs.updateClob(columnLabel, x)
  def updateClob(columnIndex: Int, x: Clob): Unit = rs.updateClob(columnIndex, x)
  def updateBlob(columnLabel: String, x: Blob): Unit = rs.updateBlob(columnLabel, x)
  def updateBlob(columnIndex: Int, x: Blob): Unit = rs.updateBlob(columnIndex, x)
  def updateRef(columnLabel: String, x: Ref): Unit = rs.updateRef(columnLabel, x)
  def updateRef(columnIndex: Int, x: Ref): Unit = rs.updateRef(columnIndex, x)
  def getURL(columnLabel: String): URL = rs.getURL(columnLabel)
  def getURL(columnIndex: Int): URL = rs.getURL(columnIndex)
  def getTimestamp(columnLabel: String, cal: Calendar): Timestamp = rs.getTimestamp(columnLabel, cal)
  def getTimestamp(columnIndex: Int, cal: Calendar): Timestamp = rs.getTimestamp(columnIndex, cal)
  def getTime(columnLabel: String, cal: Calendar): Time = rs.getTime(columnLabel, cal)
  def getTime(columnIndex: Int, cal: Calendar): Time = rs.getTime(columnIndex, cal)
  def getDate(columnLabel: String, cal: Calendar): Date = rs.getDate(columnLabel, cal)
  def getDate(columnIndex: Int, cal: Calendar): Date = rs.getDate(columnIndex, cal)
  def getArray(columnLabel: String): SQLArray = rs.getArray(columnLabel)
  def getClob(columnLabel: String): Clob = rs.getClob(columnLabel)
  def getBlob(columnLabel: String): Blob = rs.getBlob(columnLabel)
  def getRef(columnLabel: String): Ref = rs.getRef(columnLabel)
  def getObject(columnLabel: String, map: Map[String, Class[_]]): AnyRef = rs.getObject(columnLabel, map)
  def getArray(columnIndex: Int): SQLArray = rs.getArray(columnIndex)
  def getClob(columnIndex: Int): Clob = rs.getClob(columnIndex)
  def getBlob(columnIndex: Int): Blob = rs.getBlob(columnIndex)
  def getRef(columnIndex: Int): Ref = rs.getRef(columnIndex)
  def getObject(columnIndex: Int, map: Map[String, Class[_]]): AnyRef = rs.getObject(columnIndex, map)
  def getStatement: Statement = rs.getStatement
  def moveToCurrentRow(): Unit = rs.moveToCurrentRow
  def moveToInsertRow(): Unit = rs.moveToInsertRow
  def cancelRowUpdates(): Unit = rs.cancelRowUpdates
  def refreshRow(): Unit = rs.refreshRow
  def deleteRow(): Unit = rs.deleteRow
  def updateRow(): Unit = rs.updateRow
  def insertRow(): Unit = rs.insertRow
  def updateObject(columnLabel: String, x: scala.Any): Unit = rs.updateObject(columnLabel, x)
  def updateObject(columnLabel: String, x: scala.Any, scaleOrLength: Int): Unit = rs
    .updateObject(columnLabel, x, scaleOrLength)
  def updateCharacterStream(columnLabel: String, reader: Reader, length: Int): Unit = rs
    .updateCharacterStream(columnLabel, reader, length)
  def updateBinaryStream(columnLabel: String, x: InputStream, length: Int): Unit = rs
    .updateBinaryStream(columnLabel, x, length)
  def updateAsciiStream(columnLabel: String, x: InputStream, length: Int): Unit = rs
    .updateAsciiStream(columnLabel, x, length)
  def updateTimestamp(columnLabel: String, x: Timestamp): Unit = rs.updateTimestamp(columnLabel, x)
  def updateTime(columnLabel: String, x: Time): Unit = rs.updateTime(columnLabel, x)
  def updateDate(columnLabel: String, x: Date): Unit = rs.updateDate(columnLabel, x)
  def updateBytes(columnLabel: String, x: Array[Byte]): Unit = rs.updateBytes(columnLabel, x)
  def updateString(columnLabel: String, x: String): Unit = rs.updateString(columnLabel, x)
  def updateBigDecimal(columnLabel: String, x: BigDecimal): Unit = rs.updateBigDecimal(columnLabel, x)
  def updateDouble(columnLabel: String, x: Double): Unit = rs.updateDouble(columnLabel, x)
  def updateFloat(columnLabel: String, x: Float): Unit = rs.updateFloat(columnLabel, x)
  def updateLong(columnLabel: String, x: Long): Unit = rs.updateLong(columnLabel, x)
  def updateInt(columnLabel: String, x: Int): Unit = rs.updateInt(columnLabel, x)
  def updateShort(columnLabel: String, x: Short): Unit = rs.updateShort(columnLabel, x)
  def updateByte(columnLabel: String, x: Byte): Unit = rs.updateByte(columnLabel, x)
  def updateBoolean(columnLabel: String, x: Boolean): Unit = rs.updateBoolean(columnLabel, x)
  def updateNull(columnLabel: String): Unit = rs.updateNull(columnLabel)
  def updateObject(columnIndex: Int, x: scala.Any): Unit = rs.updateObject(columnIndex, x)
  def updateObject(columnIndex: Int, x: scala.Any, scaleOrLength: Int): Unit = rs
    .updateObject(columnIndex, x, scaleOrLength)
  def updateCharacterStream(columnIndex: Int, x: Reader, length: Int): Unit = rs
    .updateCharacterStream(columnIndex, x, length)
  def updateBinaryStream(columnIndex: Int, x: InputStream, length: Int): Unit = rs
    .updateBinaryStream(columnIndex, x, length)
  def updateAsciiStream(columnIndex: Int, x: InputStream, length: Int): Unit = rs
    .updateAsciiStream(columnIndex, x, length)
  def updateTimestamp(columnIndex: Int, x: Timestamp): Unit = rs.updateTimestamp(columnIndex, x)
  def updateTime(columnIndex: Int, x: Time): Unit = rs.updateTime(columnIndex, x)
  def updateDate(columnIndex: Int, x: Date): Unit = rs.updateDate(columnIndex, x)
  def updateBytes(columnIndex: Int, x: Array[Byte]): Unit = rs.updateBytes(columnIndex, x)
  def updateString(columnIndex: Int, x: String): Unit = rs.updateString(columnIndex, x)
  def updateBigDecimal(columnIndex: Int, x: BigDecimal): Unit = rs.updateBigDecimal(columnIndex, x)
  def updateDouble(columnIndex: Int, x: Double): Unit = rs.updateDouble(columnIndex, x)
  def updateFloat(columnIndex: Int, x: Float): Unit = rs.updateFloat(columnIndex, x)
  def updateLong(columnIndex: Int, x: Long): Unit = rs.updateLong(columnIndex, x)
  def updateInt(columnIndex: Int, x: Int): Unit = rs.updateInt(columnIndex, x)
  def updateShort(columnIndex: Int, x: Short): Unit = rs.updateShort(columnIndex, x)
  def updateByte(columnIndex: Int, x: Byte): Unit = rs.updateByte(columnIndex, x)
  def updateBoolean(columnIndex: Int, x: Boolean): Unit = rs.updateBoolean(columnIndex, x)
  def updateNull(columnIndex: Int): Unit = rs.updateNull(columnIndex)
  def rowDeleted(): Boolean = rs.rowDeleted
  def rowInserted(): Boolean = rs.rowInserted
  def rowUpdated(): Boolean = rs.rowUpdated
  def getConcurrency: Int = rs.getConcurrency
  def getType: Int = rs.getType
  def getFetchSize: Int = rs.getFetchSize
  def setFetchSize(rows: Int): Unit = rs.setFetchSize(rows)
  def getFetchDirection: Int = rs.getFetchDirection
  def setFetchDirection(direction: Int): Unit = rs.setFetchDirection(direction)
  def previous(): Boolean = rs.previous
  def relative(rows: Int): Boolean = rs.relative(rows)
  def absolute(row: Int): Boolean = rs.absolute(row)
  def getRow: Int = rs.getRow
  def last(): Boolean = rs.last
  def first(): Boolean = rs.first
  def afterLast(): Unit = rs.afterLast
  def beforeFirst(): Unit = rs.beforeFirst
  def isLast: Boolean = rs.isLast
  def isFirst: Boolean = rs.isFirst
  def isAfterLast: Boolean = rs.isAfterLast
  def isBeforeFirst: Boolean = rs.isBeforeFirst
  def getBigDecimal(columnLabel: String): BigDecimal = rs.getBigDecimal(columnLabel)
  def getBigDecimal(columnIndex: Int): BigDecimal = rs.getBigDecimal(columnIndex)
  def getCharacterStream(columnLabel: String): Reader = rs.getCharacterStream(columnLabel)
  def getCharacterStream(columnIndex: Int): Reader = rs.getCharacterStream(columnIndex)
  def findColumn(columnLabel: String): Int = rs.findColumn(columnLabel)
  def getObject(columnLabel: String): AnyRef = rs.getObject(columnLabel)
  def getObject(columnIndex: Int): AnyRef = rs.getObject(columnIndex)
  def getMetaData: ResultSetMetaData = rs.getMetaData
  def getCursorName: String = rs.getCursorName
  def clearWarnings(): Unit = rs.clearWarnings
  def getWarnings: SQLWarning = rs.getWarnings
  def getBinaryStream(columnLabel: String): InputStream = rs.getBinaryStream(columnLabel)
  @deprecated("parent method is deprecated", "2.1")
  def getUnicodeStream(columnLabel: String): InputStream = rs.getUnicodeStream(columnLabel)
  def getAsciiStream(columnLabel: String): InputStream = rs.getAsciiStream(columnLabel)
  def getTimestamp(columnLabel: String): Timestamp = rs.getTimestamp(columnLabel)
  def getTime(columnLabel: String): Time = rs.getTime(columnLabel)
  def getDate(columnLabel: String): Date = rs.getDate(columnLabel)
  def getBytes(columnLabel: String): Array[Byte] = rs.getBytes(columnLabel)
  @deprecated("parent method is deprecated", "2.1")
  def getBigDecimal(columnLabel: String, scale: Int): BigDecimal = rs.getBigDecimal(columnLabel, scale)
  def getDouble(columnLabel: String): Double = rs.getDouble(columnLabel)
  def getFloat(columnLabel: String): Float = rs.getFloat(columnLabel)
  def getLong(columnLabel: String): Long = rs.getLong(columnLabel)
  def getInt(columnLabel: String): Int = rs.getInt(columnLabel)
  def getShort(columnLabel: String): Short = rs.getShort(columnLabel)
  def getByte(columnLabel: String): Byte = rs.getByte(columnLabel)
  def getBoolean(columnLabel: String): Boolean = rs.getBoolean(columnLabel)
  def getString(columnLabel: String): String = rs.getString(columnLabel)
  def getBinaryStream(columnIndex: Int): InputStream = rs.getBinaryStream(columnIndex)
  @deprecated("parent method is deprecated", "2.1")
  def getUnicodeStream(columnIndex: Int): InputStream = rs.getUnicodeStream(columnIndex)
  def getAsciiStream(columnIndex: Int): InputStream = rs.getAsciiStream(columnIndex)
  def getTimestamp(columnIndex: Int): Timestamp = rs.getTimestamp(columnIndex)
  def getTime(columnIndex: Int): Time = rs.getTime(columnIndex)
  def getDate(columnIndex: Int): Date = rs.getDate(columnIndex)
  def getBytes(columnIndex: Int): Array[Byte] = rs.getBytes(columnIndex)
  @deprecated("parent method is deprecated", "2.1")
  def getBigDecimal(columnIndex: Int, scale: Int): BigDecimal = rs.getBigDecimal(columnIndex, scale)
  def getDouble(columnIndex: Int): Double = rs.getDouble(columnIndex)
  def getFloat(columnIndex: Int): Float = rs.getFloat(columnIndex)
  def getLong(columnIndex: Int): Long = rs.getLong(columnIndex)
  def getInt(columnIndex: Int): Int = rs.getInt(columnIndex)
  def getShort(columnIndex: Int): Short = rs.getShort(columnIndex)
  def getByte(columnIndex: Int): Byte = rs.getByte(columnIndex)
  def getBoolean(columnIndex: Int): Boolean = rs.getBoolean(columnIndex)
  def getString(columnIndex: Int): String = rs.getString(columnIndex)
  def wasNull(): Boolean = rs.wasNull
  def close(): Unit = rs.close
}
