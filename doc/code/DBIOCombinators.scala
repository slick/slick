package com.typesafe.slick.docs

import cats.syntax.all._

import slick.jdbc.H2Profile.api._

object DBIOCombinators {
  def main(args: Array[String]): Unit = {
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

      val a2: DBIO[Int] = ins1.andThen(ins2)

      val a3: DBIO[(Int, Int)] = ins1.zip(ins2)

      val a4: DBIO[Vector[Int]] = DBIO.sequence(Vector(ins1, ins2))
      //#combinators1

      ()
    }
    ;{
      //#cats
      // Values typed with DBIO or DBIOEffect, and the results of Slick's own combinators, have
      // cats instances. toAction gives a profile action the DBIOEffect type with its effect.
      val inserts: DBIOEffect[Effect.Write, List[Int]] =
        List(("Colombian", 7.99), ("French_Roast", 8.99)).traverse(c => (coffees += c).toAction)

      val counts: DBIOEffect[Effect.Read, (Int, Int)] =
        (coffees.length.result.toAction, coffees.filter(_.price > 8.0).length.result.toAction).tupled

      // |+| runs both actions in order and combines the results with the Semigroup of Int
      val rows: DBIOEffect[Effect.Write, Int] =
        (coffees += ("Java", 6.99)).toAction |+| (coffees += ("Espresso", 9.99)).toAction

      // The results compose with Slick's combinators like any other action; DBIO accepts every effect
      val program: DBIO[(Int, (Int, Int))] = for {
        n <- inserts
        _ <- rows
        c <- counts
      } yield (n.sum, c)
      //#cats

      ()
    }
  }
}
