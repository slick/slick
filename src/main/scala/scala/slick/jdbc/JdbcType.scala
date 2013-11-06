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

  def nullable = false

  trait OptionTypeDef extends JdbcType[Option[T]] with OptionTypedType[T]  {
    val elementType = self
    def sqlType = self.sqlType
    override def sqlTypeName = self.sqlTypeName
    def scalaType = new ScalaOptionType[T](self.scalaType)
    def setValue(v: Option[T], p: PositionedParameters) = self.setOption(v, p)
    def setOption(v: Option[Option[T]], p: PositionedParameters) = self.setOption(v.getOrElse(None), p)
    def nextValue(r: PositionedResult) = self.nextOption(r)
    def updateValue(v: Option[T], r: PositionedResult) = self.updateOption(v, r)
    override def nullable = true
    override def toString = s"Option[$self]"
    def mapChildren(f: Type => Type): OptionTypedType[T] with JdbcType[Option[T]] = {
      val e2 = f(elementType)
      if(e2 eq elementType) this
      else e2.asInstanceOf[JdbcType[T]].optionType
    }
  }
  type OptionType = OptionTypeDef
  override def optionType: OptionType = new OptionTypeDef{}

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

/** Indicates that values of this type have a literal representation in
  * SQL statements.
  * QueryBuilder (and driver-specific subclasses thereof) uses this
  * to treat LiteralNodes as volatile (i.e. using bind variables) as needed. */
trait HasLiteralForm[T]{
  /** Convert a value to a SQL literal. */
  def valueToSQLLiteral(value: T): String
}

abstract class MappedJdbcType[T, U](implicit protected val tmd: JdbcType[U], tag: ClassTag[T]) extends JdbcType[T] {
  def map(t: T): U
  def comap(u: U): T

  def newSqlType: Option[Int] = None
  def newSqlTypeName: Option[String] = None
  def newNullable: Option[Boolean] = None

  def sqlType = newSqlType.getOrElse(tmd.sqlType)
  override def sqlTypeName = newSqlTypeName.getOrElse(tmd.sqlTypeName)
  def setValue(v: T, p: PositionedParameters) = tmd.setValue(map(v), p)
  def setOption(v: Option[T], p: PositionedParameters) = tmd.setOption(v.map(map _), p)
  def nextValue(r: PositionedResult) = comap(tmd.nextValue(r))
  override def nextValueOrElse(d: =>T, r: PositionedResult) = { val v = tmd.nextValue(r); if(r.rs.wasNull) d else comap(v) }
  override def nextOption(r: PositionedResult): Option[T] = { val v = tmd.nextValue(r); if(r.rs.wasNull) None else Some(comap(v)) }
  def updateValue(v: T, r: PositionedResult) = tmd.updateValue(map(v), r)
  override def nullable = newNullable.getOrElse(tmd.nullable)
  def scalaType = ScalaBaseType[T]
}

case class MappedBaseJdbcType[T : ClassTag, U : JdbcType](tmap: T => U, tcomap: U => T) extends MappedJdbcType[T,U] with BaseTypedType[T]{
  def map(t: T) = tmap(t)
  def comap(u: U) = tcomap(u)
}

object MappedJdbcType {
  def baseWithLiteral[T : ClassTag, U : JdbcType](tmap: T => U, tcomap: U => T)(implicit literal: HasLiteralForm[U]) =
    new MappedBaseJdbcType(tmap, tcomap) with HasLiteralForm[T]{
      def valueToSQLLiteral(value: T) = literal.valueToSQLLiteral(map(value))
    }
  def base[T, U](tmap: T => U, tcomap: U => T)(implicit ttag: ClassTag[T], utype: JdbcType[U]) = utype match{
    case hasLiteral:HasLiteralForm[U]@unchecked => baseWithLiteral(tmap,tcomap)(ttag,utype,hasLiteral)
    case _ => MappedBaseJdbcType(tmap, tcomap)
  }
}
