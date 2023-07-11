package com.typesafe.slick.docs

import slick.jdbc.H2Profile.api._

object DBIOCombinators extends App {
  class Coffees(tag: Tag) extends Table[(String, Double)](tag, "COFFEES") {
    def name = column[String]("COF_NAME", O.PrimaryKey)
    def price = column[Double]("PRICE")
    def * = (name, price)
  }
  val coffees = TableQuery[Coffees]
  ;{
    //#combinators1
    val ins1: DBIO[Int] = coffees += ("Colombian", 7.99)
    val ins2: DBIO[Int] = coffees += ("French_Roast", 8.99)

    val a1: DBIO[Unit] = DBIO.seq(ins1, ins2)

    val a2: DBIO[Int] = ins1 andThen ins2

    val a3: DBIO[(Int, Int)] = ins1 zip ins2

    val a4: DBIO[Vector[Int]] = DBIO.sequence(Vector(ins1, ins2))
    //#combinators1

    ()
  }
}
