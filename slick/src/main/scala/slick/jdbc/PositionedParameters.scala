package slick.jdbc

import java.sql.{PreparedStatement, Date, Time, Timestamp, Types, Blob, Clob}

/** A wrapper for a JDBC `PreparedStatement` which allows inceremental setting of
  * parameters without having to sepcify the column index each time. */
class PositionedParameters(val ps: PreparedStatement) {

  var pos = 0

  /** Set the next parameter of the specified type, provided that a
    * `SetParameter` instance is available for it. */
  def >> [T](value: T)(implicit f: SetParameter[T]): Unit = f(value, this)

  /** Set the next parameter to SQL NULL with the specified SQL type code. */
  def setNull(sqlType: Int)            { val npos = pos + 1; ps.setNull(npos, sqlType);     pos = npos }

  /** Set the next parameter */
  def setBoolean(value: Boolean)       { val npos = pos + 1; ps.setBoolean   (npos, value); pos = npos }
  /** Set the next parameter */
  def setBlob(value: Blob)             { val npos = pos + 1; ps.setBlob      (npos, value); pos = npos }
  /** Set the next parameter */
  def setByte(value: Byte)             { val npos = pos + 1; ps.setByte      (npos, value); pos = npos }
  /** Set the next parameter */
  def setBytes(value: Array[Byte])     { val npos = pos + 1; ps.setBytes     (npos, value); pos = npos }
  /** Set the next parameter */
  def setClob(value: Clob)             { val npos = pos + 1; ps.setClob      (npos, value); pos = npos }
  /** Set the next parameter */
  def setDate(value: Date)             { val npos = pos + 1; ps.setDate      (npos, value); pos = npos }
  /** Set the next parameter */
  def setDouble(value: Double)         { val npos = pos + 1; ps.setDouble    (npos, value); pos = npos }
  /** Set the next parameter */
  def setFloat(value: Float)           { val npos = pos + 1; ps.setFloat     (npos, value); pos = npos }
  /** Set the next parameter */
  def setInt(value: Int)               { val npos = pos + 1; ps.setInt       (npos, value); pos = npos }
  /** Set the next parameter */
  def setLong(value: Long)             { val npos = pos + 1; ps.setLong      (npos, value); pos = npos }
  /** Set the next parameter */
  def setShort(value: Short)           { val npos = pos + 1; ps.setShort     (npos, value); pos = npos }
  /** Set the next parameter */
  def setString(value: String)         { val npos = pos + 1; ps.setString    (npos, value); pos = npos }
  /** Set the next parameter */
  def setTime(value: Time)             { val npos = pos + 1; ps.setTime      (npos, value); pos = npos }
  /** Set the next parameter */
  def setTimestamp(value: Timestamp)   { val npos = pos + 1; ps.setTimestamp (npos, value); pos = npos }
  /** Set the next parameter */
  def setBigDecimal(value: BigDecimal) { val npos = pos + 1; ps.setBigDecimal(npos, value.bigDecimal); pos = npos }
  /** Set the next parameter to an object of a driver-specific type that
    * corresponds to the specified SQL type code. */
  def setObject(value: AnyRef, sqlType: Int) { val npos = pos + 1; ps.setObject(npos, value, sqlType); pos = npos }

  /** Set the next parameter to the specified value or a properly typed SQL NULL */
  def setBooleanOption(value: Option[Boolean]) {
    val npos = pos + 1
    if(value eq None) ps.setNull(npos, Types.BOOLEAN) else ps.setBoolean(npos, value.get)
    pos = npos
  }
  /** Set the next parameter to the specified value or a properly typed SQL NULL */
  def setBlobOption(value: Option[Blob]) {
    val npos = pos + 1
    if(value eq None) ps.setNull(npos, Types.BLOB) else ps.setBlob(npos, value.get)
    pos = npos
  }
  /** Set the next parameter to the specified value or a properly typed SQL NULL */
  def setByteOption(value: Option[Byte]) {
    val npos = pos + 1
    if(value eq None) ps.setNull(npos, Types.TINYINT) else ps.setByte(npos, value.get)
    pos = npos
  }
  /** Set the next parameter to the specified value or a properly typed SQL NULL */
  def setBytesOption(value: Option[Array[Byte]]) {
    val npos = pos + 1
    if(value eq None) ps.setNull(npos, Types.BLOB) else ps.setBytes(npos, value.get)
    pos = npos
  }
  /** Set the next parameter to the specified value or a properly typed SQL NULL */
  def setClobOption(value: Option[Clob]) {
    val npos = pos + 1
    if(value eq None) ps.setNull(npos, Types.CLOB) else ps.setClob(npos, value.get)
    pos = npos
  }
  /** Set the next parameter to the specified value or a properly typed SQL NULL */
  def setDateOption(value: Option[Date]) {
    val npos = pos + 1
    if(value eq None) ps.setNull(npos, Types.DATE) else ps.setDate(npos, value.get)
    pos = npos
  }
  /** Set the next parameter to the specified value or a properly typed SQL NULL */
  def setDoubleOption(value: Option[Double]) {
    val npos = pos + 1
    if(value eq None) ps.setNull(npos, Types.DOUBLE) else ps.setDouble(npos, value.get)
    pos = npos
  }
  /** Set the next parameter to the specified value or a properly typed SQL NULL */
  def setFloatOption(value: Option[Float]) {
    val npos = pos + 1
    if(value eq None) ps.setNull(npos, Types.FLOAT) else ps.setFloat(npos, value.get)
    pos = npos
  }
  /** Set the next parameter to the specified value or a properly typed SQL NULL */
  def setIntOption(value: Option[Int]) {
    val npos = pos + 1
    if(value eq None) ps.setNull(npos, Types.INTEGER) else ps.setInt(npos, value.get)
    pos = npos
  }
  /** Set the next parameter to the specified value or a properly typed SQL NULL */
  def setLongOption(value: Option[Long]) {
    val npos = pos + 1
    if(value eq None) ps.setNull(npos, Types.INTEGER) else ps.setLong(npos, value.get)
    pos = npos
  }
  /** Set the next parameter to the specified value or a properly typed SQL NULL */
  def setShortOption(value: Option[Short]) {
    val npos = pos + 1
    if(value eq None) ps.setNull(npos, Types.SMALLINT) else ps.setShort(npos, value.get)
    pos = npos
  }
  /** Set the next parameter to the specified value or a properly typed SQL NULL */
  def setStringOption(value: Option[String]) {
    val npos = pos + 1
    if(value eq None) ps.setNull(npos, Types.VARCHAR) else ps.setString(npos, value.get)
    pos = npos
  }
  /** Set the next parameter to the specified value or a properly typed SQL NULL */
  def setTimeOption(value: Option[Time]) {
    val npos = pos + 1
    if(value eq None) ps.setNull(npos, Types.TIME) else ps.setTime(npos, value.get)
    pos = npos
  }
  /** Set the next parameter to the specified value or a properly typed SQL NULL */
  def setTimestampOption(value: Option[Timestamp]) {
    val npos = pos + 1
    if(value eq None) ps.setNull(npos, Types.TIMESTAMP) else ps.setTimestamp(npos, value.get)
    pos = npos
  }
  /** Set the next parameter to the specified value or a properly typed SQL NULL */
  def setBigDecimalOption(value: Option[BigDecimal]) {
    val npos = pos + 1
    if(value eq None) ps.setNull(npos, Types.DECIMAL) else ps.setBigDecimal(npos, value.get.bigDecimal)
    pos = npos
  }
  /** Set the next parameter to the specified value or a properly typed SQL NULL */
  def setObjectOption(value: Option[AnyRef], sqlType: Int) {
    val npos = pos + 1
    if(value eq None) ps.setNull(npos, sqlType) else ps.setObject(npos, value.get, sqlType)
    pos = npos
  }
}
