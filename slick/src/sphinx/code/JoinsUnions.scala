package com.typesafe.slick.docs

import slick.driver.H2Driver.api._

object JoinsUnions extends App{

  class Suppliers(tag: Tag) extends Table[(Int, String, String, String, String, String)](tag, "SUPPLIERS") {
    def id = column[Int]("SUP_ID", O.PrimaryKey)
    def name = column[String]("SUP_NAME")
    def street = column[String]("STREET")
    def city = column[String]("CITY")
    def state = column[String]("STATE")
    def zip = column[String]("ZIP")
    def * = (id, name, street, city, state, zip)
  }
  val suppliers = TableQuery[Suppliers]

  class Coffees(tag: Tag) extends Table[(String, Int, Double, Int, Int)](tag, "COFFEES") {
    def name = column[String]("COF_NAME", O.PrimaryKey)
    def supID = column[Int]("SUP_ID")
    def price = column[Double]("PRICE")
    def sales = column[Int]("SALES")
    def total = column[Int]("TOTAL")
    def * = (name, supID, price, sales, total)
    def supplier = foreignKey("SUP_FK", supID, suppliers)(_.id)
  }
  val coffees = TableQuery[Coffees]

  //#implicitCross
  val monadicCrossJoin = for {
    c <- coffees
    s <- suppliers
  } yield (c.name, s.name)
  // compiles to SQL:
  //   select x2."COF_NAME", x3."SUP_NAME"
  //     from "COFFEES" x2, "SUPPLIERS" x3
  //#implicitCross
  println(monadicCrossJoin.result.statements.head)

  //#implicitInner
  val monadicInnerJoin = for {
    c <- coffees
    s <- suppliers if c.supID === s.id
  } yield (c.name, s.name)
  // compiles to SQL:
  //   select x2."COF_NAME", x3."SUP_NAME"
  //     from "COFFEES" x2, "SUPPLIERS" x3
  //     where x2."SUP_ID" = x3."SUP_ID"
  //#implicitInner
  println(monadicInnerJoin.result.statements.head)

  //#explicit
  val crossJoin = for {
    (c, s) <- coffees join suppliers
  } yield (c.name, s.name)
  // compiles to SQL (simplified):
  //   select x2."COF_NAME", x3."SUP_NAME" from "COFFEES" x2
  //     inner join "SUPPLIERS" x3

  val innerJoin = for {
    (c, s) <- coffees join suppliers on (_.supID === _.id)
  } yield (c.name, s.name)
  // compiles to SQL (simplified):
  //   select x2."COF_NAME", x3."SUP_NAME" from "COFFEES" x2
  //     inner join "SUPPLIERS" x3
  //     on x2."SUP_ID" = x3."SUP_ID"

  val leftOuterJoin = for {
    (c, s) <- coffees joinLeft suppliers on (_.supID === _.id)
  } yield (c.name, s.map(_.name))
  // compiles to SQL (simplified):
  //   select x2."COF_NAME", x3."SUP_NAME" from "COFFEES" x2
  //     left outer join "SUPPLIERS" x3
  //     on x2."SUP_ID" = x3."SUP_ID"

  val rightOuterJoin = for {
    (c, s) <- coffees joinRight suppliers on (_.supID === _.id)
  } yield (c.map(_.name), s.name)
  // compiles to SQL (simplified):
  //   select x2."COF_NAME", x3."SUP_NAME" from "COFFEES" x2
  //     right outer join "SUPPLIERS" x3
  //     on x2."SUP_ID" = x3."SUP_ID"

  val fullOuterJoin = for {
    (c, s) <- coffees joinFull suppliers on (_.supID === _.id)
  } yield (c.map(_.name), s.map(_.name))
  // compiles to SQL (simplified):
  //   select x2."COF_NAME", x3."SUP_NAME" from "COFFEES" x2
  //     full outer join "SUPPLIERS" x3
  //     on x2."SUP_ID" = x3."SUP_ID"
  //#explicit
  println(crossJoin.result.statements.head)
  println(innerJoin.result.statements.head)
  println(leftOuterJoin.result.statements.head)
  println(rightOuterJoin.result.statements.head)
  println(fullOuterJoin.result.statements.head)

  //#zip
  val zipJoinQuery = for {
    (c, s) <- coffees zip suppliers
  } yield (c.name, s.name)

  val zipWithJoin = for {
    res <- coffees.zipWith(suppliers, (c: Coffees, s: Suppliers) => (c.name, s.name))
  } yield res
  //#zip
  //println(zipJoinQuery.result.statements.head)
  //println(zipWithJoin.result.statements.head)

  //#zipWithIndex
  val zipWithIndexJoin = for {
    (c, idx) <- coffees.zipWithIndex
  } yield (c.name, idx)
  //#zipWithIndex
  //println(zipWithIndexJoin.result.statements.head)

  //#union
  val q1 = coffees.filter(_.price < 8.0)
  val q2 = coffees.filter(_.price > 9.0)

  val unionQuery = q1 union q2
  // compiles to SQL (simplified):
  //   select x8."COF_NAME", x8."SUP_ID", x8."PRICE", x8."SALES", x8."TOTAL"
  //     from "COFFEES" x8
  //     where x8."PRICE" < 8.0
  //   union select x9."COF_NAME", x9."SUP_ID", x9."PRICE", x9."SALES", x9."TOTAL"
  //     from "COFFEES" x9
  //     where x9."PRICE" > 9.0

  val unionAllQuery = q1 ++ q2
  // compiles to SQL (simplified):
  //   select x8."COF_NAME", x8."SUP_ID", x8."PRICE", x8."SALES", x8."TOTAL"
  //     from "COFFEES" x8
  //     where x8."PRICE" < 8.0
  //   union all select x9."COF_NAME", x9."SUP_ID", x9."PRICE", x9."SALES", x9."TOTAL"
  //     from "COFFEES" x9
  //     where x9."PRICE" > 9.0
  //#union
  println(unionQuery.result.statements.head)
  println(unionAllQuery.result.statements.head)
}
