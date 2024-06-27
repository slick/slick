package com.typesafe.slick.docs

import slick.jdbc.H2Profile.api._

object Cookbook {

  object MoreThan22FieldsPattern {
   
    //#imports22
    import slick.collection.heterogeneous._
    import slick.collection.heterogeneous.syntax._
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
  }

  object DistinguishDBIOActionsByEffectTypeAtRuntime extends App {

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
        implicit object ReadWithTransactional extends ReadOnly[Effect.Read with Effect.Transactional] {
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
    run(action: DBIOAction[Int, NoStream, Effect.Read with Effect.Transactional])
    run(action: DBIOAction[Int, NoStream, Effect with Effect.Read with Effect.Transactional])
    run(action: DBIOAction[Int, NoStream, Effect.Transactional with Effect.Read with Effect])

    // Other actions.
    // "Action is not read-only, run it on the primary"
    run(action: DBIOAction[Int, NoStream, Effect])
    run(action: DBIOAction[Int, NoStream, Effect.Write])
    run(action: DBIOAction[Int, NoStream, Effect.Schema])
    run(action: DBIOAction[Int, NoStream, Effect.All])
    //#exampleDistinguishDBIOActionsByEffectTypeAtRuntime
  }
}
