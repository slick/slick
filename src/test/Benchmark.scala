package test

import com.novocode.squery.combinator.{Table, Join, NamingContext}
import com.novocode.squery.combinator.sql.QueryBuilder
import com.novocode.squery.combinator.Implicit._

object Benchmark {

  val COUNT = 20000
  val PRE_COUNT = 2000

  def main(args: Array[String]) {
    for(i <- 0 to COUNT) test1(i == 0)
    val t0 = System.nanoTime()
    for(i <- 0 to COUNT) test1(false)
    val t1 = System.nanoTime()
    val total = (t1-t0)/1000000.0
    println(COUNT+" runs tooks "+total+" ms ("+(total*1000.0/COUNT)+" µs per run)")
  }

  object Users extends Table[(Integer, String, String)]("users") {
    def id = intColumn("id")
    def first = stringColumn("first")
    def last = stringColumn("last")
    def * = id ~ first ~ last
  }

  object Orders extends Table[(Integer, Integer)]("orders") {
    def userID = intColumn("userID")
    def orderID = intColumn("orderID")
    def * = userID ~ orderID
  }

  def test1(print: Boolean) {
    val q1 = for(u <- Users) yield u
    val q2 = for {
      u <- Users
      o <- Orders where { o => (u.id is o.userID) && (u.first isNot null) }
    } yield u.first ~ u.last ~ o.orderID
    val q3 = for(u <- Users where(_.id is 42)) yield u.first ~ u.last
    val q4 = for {
      uo <- Users join Orders
      val Join(u,o) = uo
    } yield u.first ~ o.orderID sortBy u.last
    val q5 = for (
      o <- Orders
        where { o => o.orderID is queryToSubQuery(for { o2 <- Orders where(o.userID is _.userID) } yield o2.orderID.max) }
    ) yield o.orderID

    val s1 = QueryBuilder.buildSelect(q1, NamingContext())
    val s2 = QueryBuilder.buildSelect(q2, NamingContext())
    val s3 = QueryBuilder.buildSelect(q3, NamingContext())
    val s4 = QueryBuilder.buildSelect(q4, NamingContext())
    val s5 = QueryBuilder.buildSelect(q5, NamingContext())

    if(print) {
      println("q1: " + s1)
      println("q2: " + s2)
      println("q3: " + s3)
      println("q4: " + s4)
      println("q5: " + s5)
    }
  }
}
