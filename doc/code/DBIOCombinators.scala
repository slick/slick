package com.typesafe.slick.docs

import cats.effect.unsafe.implicits.global
import cats.syntax.all._

import slick.cats.Database
import slick.jdbc.DatabaseConfig
import slick.jdbc.H2Profile
import slick.jdbc.H2Profile.api._

object DBIOCombinators {
  def main(args: Array[String]): Unit = {
    // A database of its own, so that a failed check cannot leave tables behind for the other doctests
    val dc = DatabaseConfig.forURL(H2Profile, "jdbc:h2:mem:dbiocombinators", driver = "org.h2.Driver", keepAliveConnection = true)
    val (db, closeDb) = Database.resource(dc).allocated.unsafeRunSync()
    try runExamples(db)
    finally closeDb.unsafeRunSync()
  }

  private def runExamples(db: Database): Unit = {
    class Coffees(tag: Tag) extends Table[(String, Double)](tag, "COFFEES") {
      def name = column[String]("COF_NAME", O.PrimaryKey)
      def price = column[Double]("PRICE")
      def * = (name, price)
    }
    val coffees = TableQuery[Coffees]

    // The checks after each example run its actions and check what the manual says about them
    def run[R](a: DBIOAction[R, NoStream, Nothing]): R = db.run(a).unsafeRunSync()
    def names(): Set[String] = run(coffees.map(_.name).result).toSet
    def reset(): Unit = run(coffees.delete): Unit

    run(coffees.schema.create)
    ;{
      //#combinators1
      val ins1: DBIO[Int] = coffees += ("Colombian", 7.99)
      val ins2: DBIO[Int] = coffees += ("French_Roast", 8.99)

      val a1: DBIO[Unit] = DBIO.seq(ins1, ins2)

      val a2: DBIO[Int] = ins1.andThen(ins2)

      val a3: DBIO[(Int, Int)] = ins1.zip(ins2)

      val a4: DBIO[Vector[Int]] = DBIO.sequence(Vector(ins1, ins2))
      //#combinators1

      //#flatMap
      val insertIfMissing: DBIO[Boolean] =
        coffees.filter(_.name === "Colombian").exists.result.flatMap {
          case true  => DBIO.successful(false) // nothing to do
          case false => (coffees += ("Colombian", 7.99)).map(_ => true)
        }
      //#flatMap

      // DBIO.seq discards the results, andThen keeps the second one, zip both, DBIO.sequence all of them
      assert(run(a1) == ())
      assert(names() == Set("Colombian", "French_Roast"))
      reset()
      assert(run(a2) == 1)
      assert(names() == Set("Colombian", "French_Roast"))
      reset()
      assert(run(a3) == ((1, 1)))
      reset()
      assert(run(a4) == Vector(1, 1))
      reset()

      // The insert runs only if the row is missing
      assert(run(insertIfMissing))
      assert(!run(insertIfMissing))
      assert(run(coffees.filter(_.name === "Colombian").length.result) == 1)
      reset()
    }
    ;{
      //#defer
      var attempts = 0
      // The block is evaluated each time the action is run, not when it is built
      val countedInsert: DBIO[Int] = DBIO.defer {
        attempts += 1
        coffees += (s"Attempt $attempts", 1.0)
      }
      //#defer

      // The block runs each time the action runs, not when it is built
      assert(attempts == 0)
      run(countedInsert)
      run(countedInsert)
      assert(attempts == 2)
      assert(names() == Set("Attempt 1", "Attempt 2"))
      reset()

      // An exception thrown by the block fails the action instead of the code that builds it
      val failing: DBIO[Int] = DBIO.defer[Int, NoStream, Effect](throw new IllegalStateException("boom"))
      db.run(failing).attempt.unsafeRunSync() match {
        case Left(e: IllegalStateException) => assert(e.getMessage == "boom")
        case other                          => throw new AssertionError(s"Expected the action to fail with boom, got $other")
      }
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

      // Two rows from inserts, two from rows (whose |+| adds their row counts); 4 rows, 2 of them over 8.0
      assert(run(program) == ((2, (4, 2))))
      assert(names() == Set("Colombian", "French_Roast", "Java", "Espresso"))
    }
  }
}
