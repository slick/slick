package scala.slick.lifted

import java.sql.{Date, Time}
import TypeMapper._
import scala.slick.ast.Library

object Functions {
  /** The name of the database user, or an empty string if not supported by the DBMS */
  val user = Library.User.column[String]()

  /** The name of the database, or an empty string if not supported by the DBMS */
  val database = Library.Database.column[String]()

  /** The current date of the database server */
  val currentDate = Library.CurrentDate.column[Date]()

  /** The current time of the database server */
  val currentTime = Library.CurrentTime.column[Time]()

  /** The numeric constant for pi */
  val pi = Library.Pi.column[Double]()
}
