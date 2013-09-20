package com.typesafe.slick.docsnippets

import scala.slick.driver.H2Driver.simple._
import Database.dynamicSession

class JoinsUnions {

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

  val db: Database = null
  db withDynSession {
    //#implicitCross
    val implicitCrossJoin = for {
      c <- coffees
      s <- suppliers
    } yield (c.name, s.name)
    //#implicitCross

    //#implicitInner
    val implicitInnerJoin = for {
      c <- coffees
      s <- suppliers if c.supID === s.id
    } yield (c.name, s.name)
    //#implicitInner

    //#explicit
    val explicitCrossJoin = for {
      (c, s) <- coffees innerJoin suppliers
    } yield (c.name, s.name)

    val explicitInnerJoin = for {
      (c, s) <- coffees innerJoin suppliers on (_.supID === _.id)
    } yield (c.name, s.name)

    val explicitLeftOuterJoin = for {
      (c, s) <- coffees leftJoin suppliers on (_.supID === _.id)
    } yield (c.name, s.name.?)

    val explicitRightOuterJoin = for {
      (c, s) <- coffees rightJoin suppliers on (_.supID === _.id)
    } yield (c.name.?, s.name)

    val explicitFullOuterJoin = for {
      (c, s) <- coffees outerJoin suppliers on (_.supID === _.id)
    } yield (c.name.?, s.name.?)
    //#explicit

    //#zip
    val zipJoinQuery = for {
      (c, s) <- coffees zip suppliers
    } yield (c.name, s.name)

    val zipWithJoin = for {
      res <- coffees.zipWith(suppliers, (c: Coffees, s: Suppliers) => (c.name, s.name))
    } yield res
    //#zip

    //#zipWithIndex
    val zipWithIndexJoin = for {
      (c, idx) <- coffees.zipWithIndex
    } yield (c.name, idx)
    //#zipWithIndex

    //#union
    val q1 = coffees.filter(_.price < 8.0)
    val q2 = coffees.filter(_.price > 9.0)
    val unionQuery = q1 union q2
    val unionAllQuery = q1 ++ q2
    //#union
  }
}
