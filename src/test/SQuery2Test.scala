package test

import com.novocode.squery.combinator.{Table, Join, Query, ColumnOp, StatementCombinatorQueryInvoker, Projection}
import com.novocode.squery.combinator.sql.{QueryBuilder, InsertUpdateBuilder, DDLBuilder}
import com.novocode.squery.combinator.Implicit._

object SQuery2Test {
  def main(args: Array[String]) {

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

    val q1 = for(u <- Users) yield u

    val q1b = new StatementCombinatorQueryInvoker(q1).mapResult { case (id,f,l) => id + ". " + f + " " + l }

    val q2 = for {
      u <- Users
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
    } yield u.first ~ o.orderID sortBy u.last

    val q5 = for (
      o <- Orders
        where { o => o.orderID is queryToSubQuery(for { o2 <- Orders where(o.userID is _.userID) } yield o2.orderID.max) }
    ) yield o.orderID

    q1.dump("q1: ")
    println()
    q2.dump("q2: ")
    println()
    q3.dump("q3: ")
    println()
    q4.dump("q4: ")
    println()
    q5.dump("q5: ")

    val usersBase = Users.withOp(new ColumnOp.BaseTableQueryOp(Users))

    {
      println()
      println("m1a: " + new QueryBuilder(for {
        u <- Query(usersBase)
        r <- Query(u)
      } yield r).buildSelect)
      println("m1b: " + new QueryBuilder(Query(usersBase)).buildSelect)
    }

    {
      val f = { t:Table[_] => t.withOp(new ColumnOp.BaseTableQueryOp(t)) }
      println()
      println("m2a: "+ new QueryBuilder(for { u <- Query(Users) } yield f(u)).buildSelect)
      println("m2b: " + new QueryBuilder(Query(f(Users))).buildSelect)
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
    println("q1: " + new QueryBuilder(q1).buildSelect)
    println("q2: " + new QueryBuilder(q2).buildSelect)
    println("q3: " + new QueryBuilder(q3).buildSelect)
    println("q4: " + new QueryBuilder(q4).buildSelect)
    println("q5: " + new QueryBuilder(q5).buildSelect)

    println("Insert1: " + new InsertUpdateBuilder(Users).buildInsert)
    println("Insert2: " + new InsertUpdateBuilder(Users.first ~ Users.last).buildInsert)

    val d1 = Users.where(_.id is 42)
    val d2 = for(u <- Users where( _.id notIn Orders.map(_.userID) )) yield u
    println("d0: " + new QueryBuilder(Users).buildDelete)
    println("d1: " + new QueryBuilder(d1).buildDelete)
    println("d2: " + new QueryBuilder(d2).buildDelete)

    println(new DDLBuilder(Users).buildCreateTable)
    println(new DDLBuilder(Orders).buildCreateTable)
  }
}
