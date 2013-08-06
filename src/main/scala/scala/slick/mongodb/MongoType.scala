package scala.slick.mongodb

import scala.slick.ast._
import scala.reflect.ClassTag
import com.mongodb.casbah.Imports._
import scala.slick.profile.RelationalTypesComponent

/**
 * A MongoType object represents a Scala type that can be
 * used as a column type in the database. Implicit MongoTypes
 * for the standard types of a Mongo profile are provided by the driver.
 *
 * This came from the JDBC end rather than Memory/Heap as Heap had it easy w/ no type conversion...
 */
trait MongoType[T] extends TypedType[T] { self =>

  /**
   * The constant from org.bson.BSON that is used for type matching in the BSON level
   */
  def mongoType: Byte

  /**
   * The default name for the Mongo type used for column declarations.
   */
  def mongoTypeName: String

  /**
   * Set a value of the type for a given key
   * Overwrites it if it already exists...
   */
  def setValue(k: String, v: T)(doc: DBObject): Unit = {

  }

  /**
   * Set an Option parameter of the type for a given key
   * Overwrites it if it already exists...
   */
  def setOption(k: String, v: Option[T])(doc: DBObject): Unit


  /**
   * Indicates if a given field is nullable or not (not sure we'll carry this into mongo properly, but it helps w/ Schema management
   * & Object validation as well as "fetch as Option or not... "
   */
  def nullable: Boolean = false

  override def optionType: OptionTypedType[T] with MongoType[Option[T]] = new OptionTypedType[T] with MongoType[Option[T]] {
    val elementType = self
    def mongoType = self.mongoType
    override def mongoTypeName = self.mongoTypeName
    def scalaType = new ScalaOptionType[T](self.scalaType)
    def setValue(k: String, v: Option[T])(doc: DBObject) = self.setOption(k, v)(doc)
    def setOption(k: String, v: Option[Option[T]])(doc: DBObject) = self.setOption(k, v.getOrElse(None))(doc)
    override def nullable = true
    override def toString = s"Option[$self]"
  }

  override def toString = s"MongoType[$mongoTypeName]"
}

import org.bson.BSON

trait MongoTypesComponent extends RelationalTypesComponent { driver: MongoDriver =>

  type TypeInfo = MongoType[Any /* it's really _ but we'd have to cast it to Any anyway */]

  def typeInfoFor(t: Type): TypeInfo = ??? // JdbcTypesComponent:15

  abstract class DriverMongoType[T: ClassTag] extends MongoType[T] with BaseTypedType[T] {
    def scalaType = ScalaBaseType[T]
  }

  class MongoTypes {
    val booleanMongoType = new BooleanMongoType

    class BooleanMongoType extends MongoType[Boolean] {
      def mongoType: Byte = BSON.BOOLEAN
      def mongoTypeName: String = "Boolean"
      def scalaType = ScalaBaseType[Boolean]
    }

  }

}

