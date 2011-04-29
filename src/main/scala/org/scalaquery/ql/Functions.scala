package org.scalaquery.ql

import java.sql.{Date, Time}
import TypeMapper._

object Functions {
  /** The name of the database user, or an empty string if not supported by the DBMS */
  val user = SimpleFunction.nullary[String]("user", fn = true)

  /** The name of the database, or an empty string if not supported by the DBMS */
  val database = SimpleFunction.nullary[String]("database", fn = true)

  /** The current date of the database server */
  val currentDate = SimpleFunction.nullary[Date]("curDate", fn = true)

  /** The current time of the database server */
  val currentTime = SimpleFunction.nullary[Time]("curTime", fn = true)

  /** The numeric constant for pi */
  val pi = SimpleFunction.nullary[Double]("pi", fn = true)
}
