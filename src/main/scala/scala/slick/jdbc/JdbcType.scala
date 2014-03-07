package scala.slick.jdbc

import scala.slick.ast.BaseTypedType

/** A JdbcType object represents a Scala type that can be
  * used as a column type in the database. Implicit JdbcTypes
  * for the standard types of a profile are provided by the drivers. */
trait JdbcType[@specialized T] extends BaseTypedType[T] { self =>
  /** The constant from java.sql.Types that is used for setting parameters
    * of the type to NULL. */
  def sqlType: Int
  /** The default name for the SQL type that is used for column declarations. */
  def sqlTypeName: String
  /** Set a parameter of the type. */
  def setValue(v: T, p: PositionedParameters): Unit
  /** Set a parameter of the type to NULL. */
  def setNull(p: PositionedParameters): Unit
  /** Set an Option parameter of the type. */
  final def setOption(v: Option[T], p: PositionedParameters): Unit = v match {
    case Some(v) => setValue(v, p)
    case None => setNull(p)
  }

  /** Get a result column of the type. For reference types, SQL NULL values
    * are returned as `null`, for primitive types a default value is returned. */
  def nextValue(r: PositionedResult): T
  /** Check if the value returned by the immediately preceding call to
    * nextValue() was NULL. */
  def wasNull(r: PositionedResult): Boolean
  @deprecated("Use nextValue() and wasNull() instead", "2.1")
  final def nextValueOrElse(d: =>T, r: PositionedResult) = {
    val v = nextValue(r)
    if((v.asInstanceOf[AnyRef] eq null) || r.wasNull) d else v
  }
  @deprecated("Use nextValue() and wasNull() instead", "2.1")
  final def nextOption(r: PositionedResult): Option[T] = {
    val v = nextValue(r)
    if((v.asInstanceOf[AnyRef] eq null) || r.wasNull) None else Some(v)
  }

  /** Update a column of the type in a mutable result set. */
  def updateValue(v: T, r: PositionedResult): Unit
  /** Update a column of the type in a mutable result set with NULL. */
  def updateNull(r: PositionedResult): Unit = r.updateNull()
  @deprecated("Use updateValue() and updateNull() instead", "2.1")
  final def updateOption(v: Option[T], r: PositionedResult): Unit = v match {
    case Some(s) => updateValue(s, r)
    case None => r.updateNull()
  }

  /** Convert a value to a SQL literal.
    * This should throw a `SlickException` if `hasLiteralForm` is false. */
  def valueToSQLLiteral(value: T): String

  /** Indicates whether values of this type have a literal representation in
    * SQL statements.
    * This must return false if `valueToSQLLiteral` throws a SlickException.
    * QueryBuilder (and driver-specific subclasses thereof) uses this method
    * to treat LiteralNodes as volatile (i.e. using bind variables) as needed. */
  def hasLiteralForm: Boolean

  override def toString = {
    def cln = getClass.getName
    val pos = cln.lastIndexOf("$JdbcTypes$")
    val s = if(pos >= 0) cln.substring(pos+11) else cln
    val s2 = if(s.endsWith("JdbcType")) s.substring(0, s.length-8) else s
    s2 + "/" + sqlTypeName
  }
}
