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

}
