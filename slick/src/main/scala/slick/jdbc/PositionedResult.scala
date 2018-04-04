package slick.jdbc

import scala.language.higherKinds
import scala.collection.generic.CanBuildFrom
import java.sql.{ResultSet, Blob, Clob, Date, Time, Timestamp}
import java.io.Closeable
import slick.util.{ReadAheadIterator, CloseableIterator}

/**
 * A database result positioned at a row and column.
 */
abstract class PositionedResult(val rs: ResultSet) extends Closeable { outer =>
  protected[this] var pos = Int.MaxValue
  protected[this] val startPos = 0

  lazy val numColumns = rs.getMetaData().getColumnCount()

  final def currentPos = pos
  final def hasMoreColumns = pos < numColumns

  final def skip = { pos += 1; this }
  final def restart = { pos = startPos; this }
  final def rewind = { pos = Int.MinValue; this }

  def nextRow = {
    val ret = (pos == Int.MinValue) || rs.next
    pos = startPos
    ret
  }

  final def << [T](implicit f: GetResult[T]): T = f(this)
  final def <<? [T](implicit f: GetResult[Option[T]]): Option[T] = if(hasMoreColumns) this.<< else None

  final def nextBoolean()    = { val npos = pos + 1; val r = rs getBoolean    npos; pos = npos; r }
  final def nextBigDecimal() = { val npos = pos + 1; val r = rs getBigDecimal npos; pos = npos; if(r eq null) null else BigDecimal(r) }
  final def nextBlob()       = { val npos = pos + 1; val r = rs getBlob       npos; pos = npos; r }
  final def nextByte()       = { val npos = pos + 1; val r = rs getByte       npos; pos = npos; r }
  final def nextBytes()      = { val npos = pos + 1; val r = rs getBytes      npos; pos = npos; r }
  final def nextClob()       = { val npos = pos + 1; val r = rs getClob       npos; pos = npos; r }
  final def nextDate()       = { val npos = pos + 1; val r = rs getDate       npos; pos = npos; r }
  final def nextDouble()     = { val npos = pos + 1; val r = rs getDouble     npos; pos = npos; r }
  final def nextFloat()      = { val npos = pos + 1; val r = rs getFloat      npos; pos = npos; r }
  final def nextInt()        = { val npos = pos + 1; val r = rs getInt        npos; pos = npos; r }
  final def nextLong()       = { val npos = pos + 1; val r = rs getLong       npos; pos = npos; r }
  final def nextObject()     = { val npos = pos + 1; val r = rs getObject     npos; pos = npos; r }
  final def nextShort()      = { val npos = pos + 1; val r = rs getShort      npos; pos = npos; r }
  final def nextString()     = { val npos = pos + 1; val r = rs getString     npos; pos = npos; r }
  final def nextTime()       = { val npos = pos + 1; val r = rs getTime       npos; pos = npos; r }
  final def nextTimestamp()  = { val npos = pos + 1; val r = rs getTimestamp  npos; pos = npos; r }

  final def wasNull() = rs.wasNull

  final def nextBooleanOption()    = { val npos = pos + 1; val r = rs getBoolean    npos; val rr = (if(rs.wasNull) None else Some(r)); pos = npos; rr }
  final def nextBigDecimalOption() = { val npos = pos + 1; val r = rs getBigDecimal npos; val rr = (if(rs.wasNull) None else Some(BigDecimal(r))); pos = npos; rr }
  final def nextBlobOption()       = { val npos = pos + 1; val r = rs getBlob       npos; val rr = (if(rs.wasNull) None else Some(r)); pos = npos; rr }
  final def nextByteOption()       = { val npos = pos + 1; val r = rs getByte       npos; val rr = (if(rs.wasNull) None else Some(r)); pos = npos; rr }
  final def nextBytesOption()      = { val npos = pos + 1; val r = rs getBytes      npos; val rr = (if(rs.wasNull) None else Some(r)); pos = npos; rr }
  final def nextClobOption()       = { val npos = pos + 1; val r = rs getClob       npos; val rr = (if(rs.wasNull) None else Some(r)); pos = npos; rr }
  final def nextDateOption()       = { val npos = pos + 1; val r = rs getDate       npos; val rr = (if(rs.wasNull) None else Some(r)); pos = npos; rr }
  final def nextDoubleOption()     = { val npos = pos + 1; val r = rs getDouble     npos; val rr = (if(rs.wasNull) None else Some(r)); pos = npos; rr }
  final def nextFloatOption()      = { val npos = pos + 1; val r = rs getFloat      npos; val rr = (if(rs.wasNull) None else Some(r)); pos = npos; rr }
  final def nextIntOption()        = { val npos = pos + 1; val r = rs getInt        npos; val rr = (if(rs.wasNull) None else Some(r)); pos = npos; rr }
  final def nextLongOption()       = { val npos = pos + 1; val r = rs getLong       npos; val rr = (if(rs.wasNull) None else Some(r)); pos = npos; rr }
  final def nextObjectOption()     = { val npos = pos + 1; val r = rs getObject     npos; val rr = (if(rs.wasNull) None else Some(r)); pos = npos; rr }
  final def nextShortOption()      = { val npos = pos + 1; val r = rs getShort      npos; val rr = (if(rs.wasNull) None else Some(r)); pos = npos; rr }
  final def nextStringOption()     = { val npos = pos + 1; val r = rs getString     npos; val rr = (if(rs.wasNull) None else Some(r)); pos = npos; rr }
  final def nextTimeOption()       = { val npos = pos + 1; val r = rs getTime       npos; val rr = (if(rs.wasNull) None else Some(r)); pos = npos; rr }
  final def nextTimestampOption()  = { val npos = pos + 1; val r = rs getTimestamp  npos; val rr = (if(rs.wasNull) None else Some(r)); pos = npos; rr }

  final def updateBoolean(v: Boolean): Unit =       { val npos = pos + 1; rs.updateBoolean   (npos, v); pos = npos }
  final def updateBlob(v: Blob): Unit =             { val npos = pos + 1; rs.updateBlob      (npos, v); pos = npos }
  final def updateByte(v: Byte): Unit =             { val npos = pos + 1; rs.updateByte      (npos, v); pos = npos }
  final def updateBytes(v: Array[Byte]): Unit =     { val npos = pos + 1; rs.updateBytes     (npos, v); pos = npos }
  final def updateClob(v: Clob): Unit =             { val npos = pos + 1; rs.updateClob      (npos, v); pos = npos }
  final def updateDate(v: Date): Unit =             { val npos = pos + 1; rs.updateDate      (npos, v); pos = npos }
  final def updateDouble(v: Double): Unit =         { val npos = pos + 1; rs.updateDouble    (npos, v); pos = npos }
  final def updateFloat(v: Float): Unit =           { val npos = pos + 1; rs.updateFloat     (npos, v); pos = npos }
  final def updateInt(v: Int): Unit =               { val npos = pos + 1; rs.updateInt       (npos, v); pos = npos }
  final def updateLong(v: Long): Unit =             { val npos = pos + 1; rs.updateLong      (npos, v); pos = npos }
  final def updateShort(v: Short): Unit =           { val npos = pos + 1; rs.updateShort     (npos, v); pos = npos }
  final def updateString(v: String): Unit =         { val npos = pos + 1; rs.updateString    (npos, v); pos = npos }
  final def updateTime(v: Time): Unit =             { val npos = pos + 1; rs.updateTime      (npos, v); pos = npos }
  final def updateTimestamp(v: Timestamp): Unit =   { val npos = pos + 1; rs.updateTimestamp (npos, v); pos = npos }
  final def updateBigDecimal(v: BigDecimal): Unit = { val npos = pos + 1; rs.updateBigDecimal(npos, v.bigDecimal); pos = npos }
  final def updateObject(v: AnyRef): Unit =         { val npos = pos + 1; rs.updateObject    (npos, v); pos = npos }

  final def updateBooleanOption(v: Option[Boolean]): Unit =       { val npos = pos + 1; v match { case Some(s) => rs.updateBoolean   (npos, s); case None => rs.updateNull(npos) }; pos = npos }
  final def updateBlobOption(v: Option[Blob]): Unit =             { val npos = pos + 1; v match { case Some(s) => rs.updateBlob      (npos, s); case None => rs.updateNull(npos) }; pos = npos }
  final def updateByteOption(v: Option[Byte]): Unit =             { val npos = pos + 1; v match { case Some(s) => rs.updateByte      (npos, s); case None => rs.updateNull(npos) }; pos = npos }
  final def updateBytesOption(v: Option[Array[Byte]]): Unit =     { val npos = pos + 1; v match { case Some(s) => rs.updateBytes     (npos, s); case None => rs.updateNull(npos) }; pos = npos }
  final def updateClobOption(v: Option[Clob]): Unit =             { val npos = pos + 1; v match { case Some(s) => rs.updateClob      (npos, s); case None => rs.updateNull(npos) }; pos = npos }
  final def updateDateOption(v: Option[Date]): Unit =             { val npos = pos + 1; v match { case Some(s) => rs.updateDate      (npos, s); case None => rs.updateNull(npos) }; pos = npos }
  final def updateDoubleOption(v: Option[Double]): Unit =         { val npos = pos + 1; v match { case Some(s) => rs.updateDouble    (npos, s); case None => rs.updateNull(npos) }; pos = npos }
  final def updateFloatOption(v: Option[Float]): Unit =           { val npos = pos + 1; v match { case Some(s) => rs.updateFloat     (npos, s); case None => rs.updateNull(npos) }; pos = npos }
  final def updateIntOption(v: Option[Int]): Unit =               { val npos = pos + 1; v match { case Some(s) => rs.updateInt       (npos, s); case None => rs.updateNull(npos) }; pos = npos }
  final def updateLongOption(v: Option[Long]): Unit =             { val npos = pos + 1; v match { case Some(s) => rs.updateLong      (npos, s); case None => rs.updateNull(npos) }; pos = npos }
  final def updateShortOption(v: Option[Short]): Unit =           { val npos = pos + 1; v match { case Some(s) => rs.updateShort     (npos, s); case None => rs.updateNull(npos) }; pos = npos }
  final def updateStringOption(v: Option[String]): Unit =         { val npos = pos + 1; v match { case Some(s) => rs.updateString    (npos, s); case None => rs.updateNull(npos) }; pos = npos }
  final def updateTimeOption(v: Option[Time]): Unit =             { val npos = pos + 1; v match { case Some(s) => rs.updateTime      (npos, s); case None => rs.updateNull(npos) }; pos = npos }
  final def updateTimestampOption(v: Option[Timestamp]): Unit =   { val npos = pos + 1; v match { case Some(s) => rs.updateTimestamp (npos, s); case None => rs.updateNull(npos) }; pos = npos }
  final def updateBigDecimalOption(v: Option[BigDecimal]): Unit = { val npos = pos + 1; v match { case Some(s) => rs.updateBigDecimal(npos, s.bigDecimal); case None => rs.updateNull(npos) }; pos = npos }
  final def updateObjectOption(v: Option[AnyRef]): Unit =         { val npos = pos + 1; v match { case Some(s) => rs.updateObject    (npos, s); case None => rs.updateNull(npos) }; pos = npos }

  final def updateNull(): Unit = { val npos = pos + 1; rs.updateNull(npos); pos = npos }

  /**
   * Close the ResultSet and the statement which created it.
   */
  def close(): Unit

  /**
   * Create an embedded PositionedResult which extends from the given dataPos
   * column until the end of this PositionedResult, starts at the current row
   * and ends when the discriminator predicate (which can read columns starting
   * at discriminatorPos) returns false or when this PositionedResult ends.
   */
  def view(discriminatorPos: Int, dataPos: Int, discriminator: (PositionedResult => Boolean)): PositionedResult = new PositionedResult(rs) {
    override protected[this] val startPos = dataPos
    pos = Int.MinValue
    def close(): Unit = {}
    override def nextRow = {
      def disc = {
        pos = discriminatorPos
        val ret = discriminator(this)
        pos = startPos
        ret
      }
      if(pos == Int.MinValue) disc else {
        val outerRet = outer.nextRow
        val ret = outerRet && disc
        pos = startPos
        if(!ret && outerRet) outer.rewind
        ret
      }
    }
  }

  /**
   * Create an embedded PositionedResult with a single discriminator column
   * followed by the embedded data, starting at the current position. The
   * embedded view lasts while the discriminator stays the same. If the first
   * discriminator value is NULL, the view is empty.
   */
  def view1: PositionedResult = {
    val discPos = pos
    val disc = nextObject
    view(discPos, discPos+1, { r => disc != null && disc == r.nextObject })
  }

  final def build[C[_], R](gr: GetResult[R])(implicit canBuildFrom: CanBuildFrom[Nothing, R, C[R]]): C[R] = {
    val b = canBuildFrom()
    while(nextRow) b += gr(this)
    b.result()
  }

  final def to[C[_]] = new To[C]()

  final class To[C[_]] private[PositionedResult] () {
    def apply[R](gr: GetResult[R])(implicit session: JdbcBackend#Session, canBuildFrom: CanBuildFrom[Nothing, R, C[R]]) =
      build[C, R](gr)
  }
}

/**
 * An CloseableIterator for a PositionedResult.
 */
abstract class PositionedResultIterator[+T](val pr: PositionedResult, maxRows: Int, autoClose: Boolean) extends ReadAheadIterator[T] with CloseableIterator[T] {

  private[this] var closed = false
  private[this] var readRows = 0

  def rs = pr.rs

  protected def fetchNext(): T = {
    if((readRows < maxRows || maxRows <= 0) && pr.nextRow) {
      val res = extractValue(pr)
      readRows += 1
      res
    }
    else {
      if(autoClose) close()
      finished()
    }
  }

  final def close(): Unit = {
    if(!closed) {
      pr.close()
      closed = true
    }
  }

  protected def extractValue(pr: PositionedResult): T
}
