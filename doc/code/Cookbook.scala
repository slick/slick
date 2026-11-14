package com.typesafe.slick.docs

import cats.effect.unsafe.implicits.global
import slick.cats.Database
import slick.jdbc.DatabaseConfig
import slick.jdbc.H2Profile
import slick.jdbc.H2Profile.api._

object Cookbook {
  def main(args: Array[String]): Unit = {
    MoreThan22FieldsPattern.check()
    TrackNumberOfQueryCompilations.check()
    DistinguishDBIOActionsByEffectTypeAtRuntime.main(args)
  }

  object MoreThan22FieldsPattern {
   
    //#imports22
    import slick.collection.heterogeneous._
    // import slick.collection.heterogeneous.syntax._
    //#imports22
    
    //#example22
    case class Row(id: Int, name: String /* ... as many as you like */)

    class MyTable(tag: Tag) extends Table[Row](tag, "ROW") {
      def id   = column[Int]("ID", O.PrimaryKey)
      def name = column[String]("NAME")
      /* ... as many as you like */

      def * = (id :: name /* as many as you like... */ :: HNil).mapTo[Row]
    }
    //#example22

    // The recipe is about tables with more than 22 columns, so check the pattern with 23
    case class Row23(
      c1: Int, c2: Int, c3: Int, c4: Int, c5: Int, c6: Int, c7: Int, c8: Int, c9: Int, c10: Int, c11: Int, c12: Int,
      c13: Int, c14: Int, c15: Int, c16: Int, c17: Int, c18: Int, c19: Int, c20: Int, c21: Int, c22: Int, c23: Int
    )

    class Table23(tag: Tag) extends Table[Row23](tag, "TABLE23") {
      def c1  = column[Int]("C1", O.PrimaryKey)
      def c2  = column[Int]("C2")
      def c3  = column[Int]("C3")
      def c4  = column[Int]("C4")
      def c5  = column[Int]("C5")
      def c6  = column[Int]("C6")
      def c7  = column[Int]("C7")
      def c8  = column[Int]("C8")
      def c9  = column[Int]("C9")
      def c10 = column[Int]("C10")
      def c11 = column[Int]("C11")
      def c12 = column[Int]("C12")
      def c13 = column[Int]("C13")
      def c14 = column[Int]("C14")
      def c15 = column[Int]("C15")
      def c16 = column[Int]("C16")
      def c17 = column[Int]("C17")
      def c18 = column[Int]("C18")
      def c19 = column[Int]("C19")
      def c20 = column[Int]("C20")
      def c21 = column[Int]("C21")
      def c22 = column[Int]("C22")
      def c23 = column[Int]("C23")

      def * = (
        c1 :: c2 :: c3 :: c4 :: c5 :: c6 :: c7 :: c8 :: c9 :: c10 :: c11 :: c12 :: c13 :: c14 :: c15 :: c16 :: c17 ::
          c18 :: c19 :: c20 :: c21 :: c22 :: c23 :: HNil
      ).mapTo[Row23]
    }

    def check(): Unit = {
      val rows = TableQuery[Table23]
      val row = Row23(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23)
      val dc = DatabaseConfig.forProfileConfig(H2Profile, "h2mem1")
      val read = Database.resource(dc).use { db =>
        db.run(rows.schema.create >> (rows += row) >> rows.result)
      }.unsafeRunSync()
      assert(read == Seq(row), s"Expected the inserted 23-column row back, got $read")
    }
  }

  object TrackNumberOfQueryCompilations {

    //#exampleTrackNumberOfQueryCompilations
    import slick.jdbc.JdbcProfile
    import slick.compiler._
    import java.util.concurrent.atomic.AtomicLong

    trait MyProfile extends JdbcProfile {

      // This counter can be accessed from an instance of `MyProfile`
      // and exposed as an application metric.
      final val queryCompilationCounter: AtomicLong = new AtomicLong(0)

      // This method gets called to setup the QueryCompiler.
      // We'll attach a CompilerPhase that just increments the compilation counter
      // and leave the compiler otherwise unchanged.
      override def computeQueryCompiler: QueryCompiler = {
        val phase = new Phase {
          override type State = this.type
          override val name: String = "IncrementQueryCompilationCounter"
          override def apply(state: CompilerState): CompilerState = {
            // When this Phase gets applied, it just increments the counter and
            // passes the CompilerState through unchanged.
            queryCompilationCounter.incrementAndGet()
            state
          }
        }
        super.computeQueryCompiler + phase
      }
    }
    //#exampleTrackNumberOfQueryCompilations

    object CountingH2Profile extends slick.jdbc.H2Profile with MyProfile

    def check(): Unit = {
      import CountingH2Profile.api._

      class Items(tag: Tag) extends Table[Int](tag, "ITEMS") {
        def id = column[Int]("ID")
        def * = id
      }
      val items = TableQuery[Items]

      val before = CountingH2Profile.queryCompilationCounter.get
      items.filter(_.id > 1).result.statements
      val afterFirst = CountingH2Profile.queryCompilationCounter.get
      assert(afterFirst == before + 1, s"Compiling one query should add 1 to the counter, but it went from $before to $afterFirst")
      items.map(_.id).result.statements
      val afterSecond = CountingH2Profile.queryCompilationCounter.get
      assert(afterSecond == afterFirst + 1, s"Compiling a second query should add 1 more, but it went from $afterFirst to $afterSecond")
    }
  }

  object DistinguishDBIOActionsByEffectTypeAtRuntime {
    def main(args: Array[String]): Unit = {

        //#exampleDistinguishDBIOActionsByEffectTypeAtRuntime
        import slick.dbio._

        object EffectInfo {

          /**
           * Type class which can be summoned to determine if a given DBIOAction is read-only,
           * e.g., to let us determine whether to run the DBIOAction on the primary or the secondary.
           */
          sealed trait ReadOnly[E <: Effect] {
            def isReadOnly: Boolean
          }

          /**
           * Base trait provides an instance of ReadOnly where isReadOnly = false.
           * This is extended below by the ReadOnly companion object, which overrides the instance of ReadOnly for effect
           * types where isReadOnly should be true.
           */
          sealed trait ReadOnlyFalse {

            implicit def readOnlyFalse[E <: Effect]: ReadOnly[E] = new ReadOnly[E] {
              val isReadOnly: Boolean = false
            }
          }

          /**
           * Companion object providing implementations of ReadOnly for specific types where isReadOnly = true.
           */
          object ReadOnly extends ReadOnlyFalse {

            // When Effect.Read is used by itself, isReadOnly is true.
            implicit object Read extends ReadOnly[Effect.Read] {
              val isReadOnly: Boolean = true
            }

            // When Effect.Read is used with Effect.Transactional, isReadOnly is true,
            // as a transactional read is still a read.
            implicit object ReadWithTransactional extends ReadOnly[Effect.Read & Effect.Transactional] {
              val isReadOnly: Boolean = true
            }
          }
        }

        // For brevity, this run method just demonstrates that we can distinguish the effect type at runtime.
        // In a real application, we could place this method in a class that contains a handle to the primary and a handle
        // to the secondary. Then the callers just call `run(action)` and don't worry about distinguishing the two handles.
        def run[A, E <: Effect: EffectInfo.ReadOnly](action: DBIOAction[A, NoStream, E]): Unit =
          if (implicitly[EffectInfo.ReadOnly[E]].isReadOnly) println("Action is read-only, run it on the secondary")
          else println("Action is not read-only, run it on the primary")

        // We define a generic action and narrow it to specific effect types below.
        val action = DBIO.successful(42)

        // Read-only actions.
        // Each of these prints "Action is read-only, run it on the secondary"
        run(action: DBIOAction[Int, NoStream, Effect.Read])
        run(action: DBIOAction[Int, NoStream, Effect.Read & Effect.Transactional])
        run(action: DBIOAction[Int, NoStream, Effect & Effect.Read & Effect.Transactional])
        run(action: DBIOAction[Int, NoStream, Effect.Transactional & Effect.Read & Effect])

        // Other actions.
        // "Action is not read-only, run it on the primary"
        run(action: DBIOAction[Int, NoStream, Effect])
        run(action: DBIOAction[Int, NoStream, Effect.Write])
        run(action: DBIOAction[Int, NoStream, Effect.Schema])
        run(action: DBIOAction[Int, NoStream, Effect.All])
        //#exampleDistinguishDBIOActionsByEffectTypeAtRuntime
    }
  }
}
