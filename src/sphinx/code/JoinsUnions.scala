package com.typesafe.slick.docsnippets

import scala.slick.driver.H2Driver.simple._
import Database.threadLocalSession

class JoinsUnions {

  object Suppliers extends Table[(Int, String, String, String, String, String)]("SUPPLIERS") {
    def id = column[Int]("SUP_ID", O.PrimaryKey)
    def name = column[String]("SUP_NAME")
    def street = column[String]("STREET")
    def city = column[String]("CITY")
    def state = column[String]("STATE")
    def zip = column[String]("ZIP")
    def * = id ~ name ~ street ~ city ~ state ~ zip
  }

  object Coffees extends Table[(String, Int, Double, Int, Int)]("COFFEES") {
    def name = column[String]("COF_NAME", O.PrimaryKey)
    def supID = column[Int]("SUP_ID")
    def price = column[Double]("PRICE")
    def sales = column[Int]("SALES")
    def total = column[Int]("TOTAL")
    def * = name ~ supID ~ price ~ sales ~ total
    def supplier = foreignKey("SUP_FK", supID, Suppliers)(_.id)
  }

  val db: Database = null
  db withSession {
    //#implicitCross
    val implicitCrossJoin = for {
      c <- Coffees
      s <- Suppliers
    } yield (c.name, s.name)
    //#implicitCross

    //#implicitInner
    val implicitInnerJoin = for {
      c <- Coffees
      s <- Suppliers if c.supID === s.id
    } yield (c.name, s.name)
    //#implicitInner

    //#explicit
    val explicitCrossJoin = for {
      (c, s) <- Coffees innerJoin Suppliers
    } yield (c.name, s.name)

    val explicitInnerJoin = for {
      (c, s) <- Coffees innerJoin Suppliers on (_.supID === _.id)
    } yield (c.name, s.name)

    val explicitLeftOuterJoin = for {
      (c, s) <- Coffees leftJoin Suppliers on (_.supID === _.id)
    } yield (c.name, s.name.?)

    val explicitRightOuterJoin = for {
      (c, s) <- Coffees rightJoin Suppliers on (_.supID === _.id)
    } yield (c.name.?, s.name)

    val explicitFullOuterJoin = for {
      (c, s) <- Coffees outerJoin Suppliers on (_.supID === _.id)
    } yield (c.name.?, s.name.?)
    //#explicit

    //#zip
    val zipJoinQuery = for {
      (c, s) <- Coffees zip Suppliers
    } yield (c.name, s.name)

    val zipWithJoin = for {
      res <- Coffees.zipWith(Suppliers, (c: Coffees.type, s: Suppliers.type) => (c.name, s.name))
    } yield res
    //#zip

    //#zipWithIndex
    val zipWithIndexJoin = for {
      (c, idx) <- Coffees.zipWithIndex
    } yield (c.name, idx)
    //#zipWithIndex

    //#union
    val q1 = Query(Coffees).filter(_.price < 8.0)
    val q2 = Query(Coffees).filter(_.price > 9.0)
    val unionQuery = q1 union q2
    val unionAllQuery = q1 unionAll q2
    //#union
  }
}
