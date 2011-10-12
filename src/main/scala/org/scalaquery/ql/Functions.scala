package org.scalaquery.ql

import java.sql.{Date, Time}
import TypeMapper._

object Functions {
  /** The name of the database user, or an empty string if not supported by the DBMS */
  val user = EscFunction[String]("user")

  /** The name of the database, or an empty string if not supported by the DBMS */
  val database = EscFunction[String]("database")

  /** The current date of the database server */
  val currentDate = EscFunction[Date]("curDate")

  /** The current time of the database server */
  val currentTime = EscFunction[Time]("curTime")

  /** The numeric constant for pi */
  val pi = EscFunction[Double]("pi")
}
