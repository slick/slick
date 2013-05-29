package scala.slick.jdbc

import scala.slick.ast._
import scala.reflect.ClassTag

/**
 * A JdbcType object represents a Scala type that can be
 * used as a column type in the database. Implicit JdbcTypes
 * for the standard types of a profile are provided by the drivers.
 */
trait JdbcType[T] extends TypedType[T] { self =>
  /**
   * The constant from java.sql.Types that is used for setting parameters of
   * the type to NULL.
   */
  def sqlType: Int
  /**
   * The default name for the SQL type that is used for column declarations.
   */
  def sqlTypeName: String
  /**
   * Set a parameter of the type.
   */
  def setValue(v: T, p: PositionedParameters): Unit
  /**
   * Set an Option parameter of the type.
   */
  def setOption(v: Option[T], p: PositionedParameters): Unit
  /**
   * Get a result column of the type.
   */
  def nextValue(r: PositionedResult): T
  /**
   * Update a column of the type in a mutable result set.
   */
  def updateValue(v: T, r: PositionedResult): Unit
  def nextValueOrElse(d: =>T, r: PositionedResult) = { val v = nextValue(r); if(r.rs.wasNull) d else v }
  def nextOption(r: PositionedResult): Option[T] = { val v = nextValue(r); if(r.rs.wasNull) None else Some(v) }
  def updateOption(v: Option[T], r: PositionedResult): Unit = v match {
    case Some(s) => updateValue(s, r)
    case None => r.updateNull()
  }
  def valueToSQLLiteral(value: T): String = value.toString
  def nullable = false

  override def optionType: OptionTypedType[T] with JdbcType[Option[T]] = new OptionTypedType[T] with JdbcType[Option[T]] {
    val elementType = self
    def sqlType = self.sqlType
    override def sqlTypeName = self.sqlTypeName
    def scalaType = new ScalaOptionType[T](self.scalaType)
    def setValue(v: Option[T], p: PositionedParameters) = self.setOption(v, p)
    def setOption(v: Option[Option[T]], p: PositionedParameters) = self.setOption(v.getOrElse(None), p)
    def nextValue(r: PositionedResult) = self.nextOption(r)
    def updateValue(v: Option[T], r: PositionedResult) = self.updateOption(v, r)
    override def valueToSQLLiteral(value: Option[T]): String = value.map(self.valueToSQLLiteral).getOrElse("null")
    override def nullable = true
    override def toString = s"Option[$self]"
    def mapChildren(f: Type => Type): OptionTypedType[T] with JdbcType[Option[T]] = {
      val e2 = f(elementType)
      if(e2 eq elementType) this
      else e2.asInstanceOf[JdbcType[T]].optionType
    }
  }

  override def toString = {
    def cln = getClass.getName
    val pos = cln.lastIndexOf("$JdbcTypes$")
    val s = if(pos >= 0) cln.substring(pos+11) else cln
    val s2 = if(s.endsWith("JdbcType")) s.substring(0, s.length-8) else s
    s2 + "/" + sqlTypeName
  }
}

object JdbcType {
  private[slick] lazy val typeNames = Map() ++
  (for(f <- classOf[java.sql.Types].getFields)
    yield f.get(null).asInstanceOf[Int] -> f.getName)
}

abstract class MappedJdbcType[T, U](implicit tmd: JdbcType[U], tag: ClassTag[T]) extends JdbcType[T] {
  def map(t: T): U
  def comap(u: U): T

  def newSqlType: Option[Int] = None
  def newSqlTypeName: Option[String] = None
  def newValueToSQLLiteral(value: T): Option[String] = None
  def newNullable: Option[Boolean] = None

  def sqlType = newSqlType.getOrElse(tmd.sqlType)
  override def sqlTypeName = newSqlTypeName.getOrElse(tmd.sqlTypeName)
  def setValue(v: T, p: PositionedParameters) = tmd.setValue(map(v), p)
  def setOption(v: Option[T], p: PositionedParameters) = tmd.setOption(v.map(map _), p)
  def nextValue(r: PositionedResult) = comap(tmd.nextValue(r))
  override def nextValueOrElse(d: =>T, r: PositionedResult) = { val v = tmd.nextValue(r); if(r.rs.wasNull) d else comap(v) }
  override def nextOption(r: PositionedResult): Option[T] = { val v = tmd.nextValue(r); if(r.rs.wasNull) None else Some(comap(v)) }
  def updateValue(v: T, r: PositionedResult) = tmd.updateValue(map(v), r)
  override def valueToSQLLiteral(value: T) = newValueToSQLLiteral(value).getOrElse(tmd.valueToSQLLiteral(map(value)))
  override def nullable = newNullable.getOrElse(tmd.nullable)
  def scalaType = ScalaBaseType[T]
}

object MappedJdbcType {
  def base[T : ClassTag, U : JdbcType](tmap: T => U, tcomap: U => T): JdbcType[T] with BaseTypedType[T] =
    new MappedJdbcType[T, U] with BaseTypedType[T] {
      def map(t: T) = tmap(t)
      def comap(u: U) = tcomap(u)
    }
}
