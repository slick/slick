package org.scalaquery.example

import org.scalaquery.ql.{Join, Query, Projection, ColumnBase, AbstractTable}
import org.scalaquery.ql.TypeMapper._
import org.scalaquery.ql.basic.BasicDriver
import org.scalaquery.ql.basic.BasicDriver.Implicit._
import org.scalaquery.ql.basic.{BasicTable => Table}
import org.scalaquery.util.NamingContext

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

    def dump(n: String, q: Query[ColumnBase[_]]) {
      val nc = NamingContext()
      q.dump(n+": ", nc)
      println(BasicDriver.buildSelectStatement(q, nc))
      println()
    }

    val q1 = for(u <- Users) yield u

    val q1b = q1.mapResult { case (id,f,l) => id + ". " + f + " " + l }

    val q2 = for {
      u <- Users
      _ <- Query.orderBy(u.first asc) >> Query.orderBy(u.last desc)
      o <- Orders where { o => (u.id is o.userID) && (u.first isNotNull) }
    } yield u.first ~ u.last ~ o.orderID

    val q3 = for(u <- Users where(_.id is 42)) yield u.first ~ u.last

    val q4 = for {
      Join(u, o) <- Users innerJoin Orders on (_.id is _.userID)
      _ <- Query orderBy u.last
    } yield u.first ~ o.orderID

    val q5 = for (
      o <- for ( o <- Orders if o.orderID in (for { o2 <- Orders if o.userID is o2.userID } yield o2.orderID.max) ) yield o.orderID;
      _ <- Query orderBy o
    ) yield o

    val q6a = for (
      o <- (for ( o <- Orders if o.orderID in (for { o2 <- Orders if o.userID is o2.userID } yield o2.orderID.max) ) yield o.orderID).sub;
      _ <- Query orderBy o
    ) yield o

    val q6b = for (
      o <- (for ( o <- Orders if o.orderID in (for { o2 <- Orders if o.userID is o2.userID } yield o2.orderID.max) ) yield o.orderID ~ o.userID).sub;
      _ <- Query orderBy o._1
    ) yield o

    val q6c = for (
      o <- (for ( o <- Orders if o.orderID in (for { o2 <- Orders if o.userID is o2.userID } yield o2.orderID.max) ) yield o).sub;
      _ <- Query orderBy o.orderID
    ) yield o.orderID ~ o.userID

    dump("q1", q1)
    dump("q2", q2)
    dump("q3", q3)
    dump("q4", q4)
    dump("q5", q5)
    dump("q6a", q6a)
    dump("q6b", q6b)
    dump("q6c", q6c)

    val usersBase = Users.mapOp(n => new AbstractTable.Alias(n))

    {
      val m1a = for {
        u <- Query(usersBase)
        r <- Query(u)
      } yield r
      val m1b = Query(usersBase)
      dump("m1a", m1a)
      dump("m1b", m1b)
    }

    {
      val f = { t:Table[_] => t.mapOp(n => new AbstractTable.Alias(n)) }
      val m2a = for { u <- Query(Users) } yield f(u)
      val m2b = Query(f(Users))
      dump("m2a", m2a)
      dump("m2b", m2b)
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

    println("Insert1: " + BasicDriver.buildInsertStatement(Users))
    println("Insert2: " + BasicDriver.buildInsertStatement(Users.first ~ Users.last))

    val d1 = Users.where(_.id is 42)
    val d2 = for(u <- Users where( _.id notIn Orders.map(_.userID) )) yield u
    println("d0: " + BasicDriver.buildDeleteStatement(Users, NamingContext()))
    println("d1: " + BasicDriver.buildDeleteStatement(d1, NamingContext()))
    println("d2: " + BasicDriver.buildDeleteStatement(d2, NamingContext()))

    (Users.ddl ++ Orders.ddl).createStatements.foreach(println)
  }
}
