package org.scalaquery.ql

import java.sql.{Date, Time}
import TypeMapper._

object Functions {
  /** The name of the database user */
  val user = SimpleScalarFunction.nullary[String]("user")

  /** The name of the database */
  val database = SimpleScalarFunction.nullary[String]("database")

  /** The current date of the database server */
  val currentDate = SimpleScalarFunction.nullary[Date]("curDate")

  /** The current time of the database server */
  val currentTime = SimpleScalarFunction.nullary[Time]("curTime")

  /** The numeric constant for pi */
  val pi = SimpleScalarFunction.nullary[Double]("pi")
}
