package test

import com.novocode.squery.combinator.{Table, Join, Query, StatementCombinatorQueryInvoker, Projection, NamingContext, Order}
import com.novocode.squery.combinator.sql.{QueryBuilder, InsertBuilder, DDLBuilder}
import com.novocode.squery.combinator.Implicit._
import com.novocode.squery.session.TypeMapper._

object SQuery2Test {
  def main(args: Array[String]) {

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

    val q1nc = NamingContext()
    val q2nc = NamingContext()
    val q3nc = NamingContext()
    val q4nc = NamingContext()
    val q5nc = NamingContext()
    val m1anc = NamingContext()
    val m1bnc = NamingContext()
    val m2anc = NamingContext()
    val m2bnc = NamingContext()

    val q1 = for(u <- Users) yield u

    val q1b = new StatementCombinatorQueryInvoker(q1).mapResult { case (id,f,l) => id + ". " + f + " " + l }

    val q2 = for {
      u <- Users
      _ <- Order +u.first >> Order -u.last
      o <- Orders where { o => (u.id is o.userID) && (u.first isNot null) }
    } yield u.first ~ u.last ~ o.orderID

    /*
    val (_, uLast, oID) = q2.first

    for((first, last, oID) <- q2.all) println(first + " " + last + " " + oID)
    */

    val q3 = for(u <- Users where(_.id is 42)) yield u.first ~ u.last

    val q4 = for {
      uo <- Users join Orders
      val Join(u,o) = uo
      _ <- Order +u.last
    } yield u.first ~ o.orderID

    /*val q4b = for {
      Join(u,o) <- Users join Orders
    } yield u.first ~ o.orderID sortBy u.last*/

    val q5 = for (
      o <- Orders
        where { o => o.orderID is queryToSubQuery(for { o2 <- Orders where(o.userID is _.userID) } yield o2.orderID.max) }
    ) yield o.orderID

    q1.dump("q1: ", q1nc)
    println(QueryBuilder.buildSelect(q1, q1nc))
    println()
    q2.dump("q2: ", q2nc)
    println(QueryBuilder.buildSelect(q2, q2nc))
    println()
    q3.dump("q3: ", q3nc)
    println(QueryBuilder.buildSelect(q3, q3nc))
    println()
    q4.dump("q4: ", q4nc)
    println(QueryBuilder.buildSelect(q4, q4nc))
    println()
    q5.dump("q5: ", q5nc)
    println(QueryBuilder.buildSelect(q5, q5nc))

    val usersBase = Users.mapOp(n => new Table.Alias(n))

    {
      println()
      val m1a = for {
        u <- Query(usersBase)
        r <- Query(u)
      } yield r
      val m1b = Query(usersBase)
      m1a.dump("m1a: ", m1anc)
      println()
      m1b.dump("m1b: ", m1bnc)
      println()
      println("m1a: " + QueryBuilder.buildSelect(m1a, m1anc))
      println("m1b: " + QueryBuilder.buildSelect(m1b, m1bnc))
    }

    {
      println()
      val f = { t:Table[_] => t.mapOp(n => new Table.Alias(n)) }
      val m2a = for { u <- Query(Users) } yield f(u)
      val m2b = Query(f(Users))
      m2a.dump("m2a: ", m2anc)
      println()
      m2b.dump("m2b: ", m2bnc)
      println()
      println("m2a: "+ QueryBuilder.buildSelect(m2a, m2anc))
      println("m2b: " + QueryBuilder.buildSelect(m2b, m2bnc))
    }

    /*
    {
      val g1 = { u: UsersTable => u sortBy u.first }
      val g2 = { u: UsersTable => u sortBy u.last }
      println()
      (for(a <- Query(usersBase); b <- g2(a); result <- g1(b)) yield result).dump("m3a: ")
      (for(a <- Query(usersBase); result <- (for(b <- g2(a); temp <- g1(b)) yield temp)) yield result).dump("m3b: ")
    }
    */

    println()

    println("Insert1: " + new InsertBuilder(Users).buildInsert)
    println("Insert2: " + new InsertBuilder(Users.first ~ Users.last).buildInsert)

    val d1 = Users.where(_.id is 42)
    val d2 = for(u <- Users where( _.id notIn Orders.map(_.userID) )) yield u
    println("d0: " + QueryBuilder.buildDelete(Users, NamingContext()))
    println("d1: " + QueryBuilder.buildDelete(d1, NamingContext()))
    println("d2: " + QueryBuilder.buildDelete(d2, NamingContext()))

    println(new DDLBuilder(Users).buildCreateTable)
    println(new DDLBuilder(Orders).buildCreateTable)
  }
}
