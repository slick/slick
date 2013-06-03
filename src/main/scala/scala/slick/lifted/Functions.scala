package scala.slick.lifted

import java.sql.{Date, Time}
import scala.slick.ast.{TypedType, Library}
import FunctionSymbolExtensionMethods._
import scala.slick.ast.ScalaBaseType._

object Functions {

  /** The name of the database user, or an empty string if not supported by the DBMS */
  val user = Library.User.column[String]()

  /** The name of the database, or an empty string if not supported by the DBMS */
  val database = Library.Database.column[String]()

  /** The current date of the database server */
  def currentDate(implicit tpe: TypedType[Date]) = Library.CurrentDate.column[Date]()

  /** The current time of the database server */
  def currentTime(implicit tpe: TypedType[Time]) = Library.CurrentTime.column[Time]()

  /** The numeric constant for pi */
  def pi(implicit tpe: TypedType[Double]) = Library.Pi.column[Double]()
}
