package com.novocode.squery.combinator

import java.sql.{Date, Time}
import TypeMapper._

object Functions {
  val user = SimpleScalarFunction.nullary[String]("user")
  val database = SimpleScalarFunction.nullary[String]("database")
  val currentDate = SimpleScalarFunction.nullary[Date]("curDate")
  val currentTime = SimpleScalarFunction.nullary[Time]("curTime")
  val pi = SimpleScalarFunction.nullary[Double]("pi")
}
