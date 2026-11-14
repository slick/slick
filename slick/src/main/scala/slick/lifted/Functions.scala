package slick.lifted

import java.sql.{Date, Time}
import slick.ast.{Library, TypedType}
import FunctionSymbolExtensionMethods.*
import slick.ast.ScalaBaseType.*
import slick.basic.Capability
import slick.relational.RelationalCapabilities

/** Contains stand-alone database functions for use in queries. Functions which
  * operate on columns are generally added as extension methods to the appropriate
  * column types instead. */
object Functions {

  /** The name of the database user, or an empty string if not supported by the DBMS */
  def user(implicit cap: Capability.UnknownProfile[RelationalCapabilities.functionUser.type]): Rep[String] =
    Library.User.column[String]()

  /** The name of the database, or an empty string if not supported by the DBMS */
  def database(implicit cap: Capability.UnknownProfile[RelationalCapabilities.functionDatabase.type]): Rep[String] =
    Library.Database.column[String]()

  /** The current date of the database server */
  def currentDate(implicit tpe: TypedType[Date]) = Library.CurrentDate.column[Date]()

  /** The current time of the database server */
  def currentTime(implicit tpe: TypedType[Time]) = Library.CurrentTime.column[Time]()

  /** The numeric constant for pi */
  def pi(implicit tpe: TypedType[Double]) = Library.Pi.column[Double]()
}
