package scala.slick.mongodb

import scala.slick.ast._
import scala.reflect.ClassTag
import com.mongodb.casbah.Imports._
import scala.slick.profile.RelationalTypesComponent
import java.util.UUID

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
    doc.put(k, v)
  }

  /**
   * Set an Option parameter of the type for a given key
   * Overwrites it if it already exists...
   */
  def setOption(k: String, v: Option[T])(doc: DBObject): Unit = v match {
    case Some(value) =>
      doc.put(k, value)
    case None => // TODO - should we set null? Or leave the key out? I like leaving the structure in
      doc.put(k, null)
  }


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
    // TODO - for now, any types that are in my chart of "not really supported" like Char/Byte are left out.
    /**
     ____ Essential Types (+ supported, - not supported, ~ "questions about supportability") ____

     Boolean +
     Char  ~ (Mongo lacks a Char type.. we might have to map on string or an int32?)
     Int +
     Long +
     Null + (There's a special 'null' type in mongo though this wouldn't be a FIELD type I assume)
     String +
     Unit ~ (Not sure what the type mapping to mongo of Unit would be?)

     _____ JDBC Types (+ supported, - not supported, ~ "questions about supportability") ____


     Boolean +
     Blob ~ ( Not a specific Mongo blob type ; We can use binary blocks, or strings)
     Clob ~ ( Not a specific Mongo clob type ; We can use binary blocks, or strings)
     Byte ~ (Mongo lacks a single byte/char type (or even short). String or int32 to map?)
     Char ~ (Mongo lacks a single byte/char type (or even short). String or int32 to map?)
     Date ~  (Mongo does not have a specific DATE type; we must use a Timestamp; including time w/ date)
     Double + (Doubles + floats both map to a Mongo double which is  [8 bytes - 64-bit iee 754 floating point])
     Float + (Doubles + floats both map to a Mongo double which is  [8 bytes - 64-bit iee 754 floating point])
     Int +
     Long +
     Short - ( There is no specific short type for mongo….)
     String +
     Time - (Mongo lacks a "time only component" type and I see no easy way to fudge it)
     Timestamp +
     Unit ~ (Not sure what the type mapping to mongo of Unit would be?)
     UUID +
     BigDecimal - (this is a tricky one, mongo really doesn't support BigDecimal safely. I'll talk to Rose Toomey [salat] and see how she is solving it these days) - not supported by relational profile ... Throw unsupported exception
    */

    val booleanMongoType = new BooleanMongoType
    val byteArrayMongoType = new ByteArrayMongoType
    val dateMongoType = new DateMongoType
    // TODO - Add a time type that stores as a mongo date w/ just the time since Jan 1, 1970?
    val doubleMongoType = new DoubleMongoType
    val floatMongoType = new FloatMongoType /** Mongo only technically supports Doubles - 64 bit ieee-754 floating point */
    val intMongoType = new IntMongoType
    val longMongoType = new LongMongoType
    val stringMongoType = new StringMongoType
    val timestampMongoType = new TimestampMongoType
    val uuidMongoType = new UUIDMongoType
    val nullMongoType = new NullMongoType

    class BooleanMongoType extends DriverMongoType[Boolean] {
      def mongoType: Byte = BSON.BOOLEAN
      def mongoTypeName: String = "Boolean"
    }

    class ByteArrayMongoType extends DriverMongoType[Array[Byte]] {
      def mongoType: Byte = BSON.BINARY
      def mongoTypeName: String = "Binary"
    }

    class DateMongoType extends DriverMongoType[java.sql.Date] {
      def mongoType: Byte = BSON.DATE
      def mongoTypeName: String = "Date"
    }

    class DoubleMongoType extends DriverMongoType[Double] {
      def mongoType: Byte = BSON.NUMBER
      def mongoTypeName: String = "Number (64-bit IEEE-754 FP aka 'Double')"
    }

    class FloatMongoType extends DriverMongoType[Double] {
      def mongoType: Byte = BSON.NUMBER
      def mongoTypeName: String = "Number (64-bit IEEE-754 FP aka 'Double' [as Float])"
    }

    class IntMongoType extends DriverMongoType[Int] {
      def mongoType: Byte = BSON.NUMBER_INT
      def mongoTypeName: String = "Int"
    }

    class LongMongoType extends DriverMongoType[Long] {
      def mongoType: Byte = BSON.NUMBER_LONG
      def mongoTypeName: String = "Long"
    }

    class StringMongoType extends DriverMongoType[String] {
      def mongoType: Byte = BSON.STRING
      def mongoTypeName: String = "String"
    }

    // TODO - I don't think we can properly store the nanoseconds permitted in java.sql.Timestamp...
    class TimestampMongoType extends DriverMongoType[java.sql.Timestamp] {
      def mongoType: Byte = BSON.DATE
      def mongoTypeName: String = "Date [as Timestamp]"
    }

    class UUIDMongoType extends DriverMongoType[UUID] {
      // TODO - We need binary subtyping.. ugh.
      def mongoType: Byte = BSON.B_UUID
      def mongoTypeName: String = "UUID"
    }

    class NullMongoType extends DriverMongoType[Null] {
      def mongoType: Byte = BSON.NULL
      def mongoTypeName: String = "Null"
    }
  }

}

