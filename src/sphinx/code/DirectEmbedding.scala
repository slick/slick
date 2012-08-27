package scala.slick.examples.direct

//#imports
import scala.slick.driver.H2Driver
import H2Driver.simple.Database
import Database.{threadLocalSession => session}
import scala.slick.direct._
import scala.slick.direct.AnnotationMapper._
//#imports

//#schema
  // describe schema for direct embedding
  @table(name="COFFEES")
  case class Coffee(
    @column(name="NAME")
    name : String,
    @column(name="PRICE")
    price : Double
  )
//#schema

object DirectEmbedding extends App {
    //#result
  val db = Database.forURL("jdbc:h2:mem:test1", driver = "org.h2.Driver")
  db withSession {
    //#result
    //#inserts
      // fill database with test data (using plain SQL)
      val coffees_data = Vector(
        ("Colombian",          1.0),
        ("French_Roast",       2.0),
        ("Espresso",           3.0),
        ("Colombian_Decaf",    4.0),
        ("French_Roast_Decaf", 5.0)
      )
      // create test table
      import scala.slick.jdbc.StaticQuery.interpolation
      sqlu"create table COFFEES(NAME varchar(255), PRICE double)".execute
      (for {
        (name, sales) <- coffees_data
      } yield sqlu"insert into COFFEES values ($name, $sales)".first).sum
    //#inserts

    //#query
      // query database using direct embedding
      val q1 = Queryable[Coffee]
      val q2 = q1.filter( _.price > 3.0 ).map( _ .name )
    //#query

    //#result
      // execute query using a chosen db backend
      val backend = new SlickBackend( H2Driver, AnnotationMapper )
      println( backend.result( q2, session ) )
      println( backend.result( q2.length, session ) )
    //#result

    //#implicitqueryable
      //
      val iq1 = ImplicitQueryable( q1, backend, session )
      val iq2 = iq1.filter( c => c.price > 3.0 )
      println( iq2.toSeq ) //  <- triggers execution 
      println( iq2.length ) // <- triggers execution
    //#implicitqueryable

    //#nesting
      q1.map( c => (c.name, (c, c.price)) )
    //#nesting
    //#result
  }
    //#result
}
