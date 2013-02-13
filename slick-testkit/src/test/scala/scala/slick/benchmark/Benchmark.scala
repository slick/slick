package scala.slick.benchmark

import scala.slick.lifted.Query
import scala.slick.driver.JdbcDriver
import scala.slick.driver.JdbcDriver.Implicit._
import scala.slick.driver.JdbcDriver.Table

object Benchmark {

  val COUNT = 2000

  def main(args: Array[String]) {
    for(i <- 0 to COUNT) test1(i == 0)
    val t0 = System.nanoTime()
    for(i <- 0 to COUNT) test1(false)
    val t1 = System.nanoTime()
    val total = (t1-t0)/1000000.0
    println(COUNT+" runs tooks "+total+" ms ("+(total*1000.0/COUNT)+" µs per run)")
  }

  object Users extends Table[(Int, String, String)]("users") {
    def id = column[Int]("id")
    def first = column[String]("first")
    def last = column[String]("last")
    def * = id ~ first ~ last
  }

  object Orders extends Table[(Int, Int)]("orders") {
    def userID = column[Int]("userID")
    def orderID = column[Int]("orderID")
    def * = userID ~ orderID
  }

  def test1(print: Boolean) {
    val q1 = for(u <- Users) yield u
    val q2 = for {
      u <- Users
      o <- Orders where { o => (u.id is o.userID) && (u.first.isNotNull) }
    } yield u.first ~ u.last ~ o.orderID
    val q3 = for(u <- Users where(_.id is 42)) yield u.first ~ u.last
    val q4 =
      (Users innerJoin Orders on (_.id is _.userID)).sortBy(_._1.last.asc).map(uo => uo._1.first ~ uo._2.orderID)
    val q5 = for (
      o <- Orders
        where { o => o.orderID === (for { o2 <- Orders where(o.userID is _.userID) } yield o2.orderID).max }
    ) yield o.orderID

    val s1 = q1.selectStatement
    val s2 = q2.selectStatement
    val s3 = q3.selectStatement
    val s4 = q4.selectStatement
    val s5 = q5.selectStatement

    if(print) {
      println("q1: " + s1)
      println("q2: " + s2)
      println("q3: " + s3)
      println("q4: " + s4)
      println("q5: " + s5)
    }
  }
}
