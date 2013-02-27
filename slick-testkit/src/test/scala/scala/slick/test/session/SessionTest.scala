package scala.slick.test.session

import scala.slick.driver.H2Driver.simple._
import org.junit.Test
import org.junit.Assert._

class SessionTest{
  @Test def test() {
    object Coffees extends Table[String]("COFFEES") {
      def name = column[String]("COF_NAME", O.PrimaryKey)
      def * = name
    }
    val db = Database.forURL("jdbc:h2:mem:test2;INIT="+Coffees.ddl.createStatements.mkString("\\;"), driver = "org.h2.Driver")
    val query = for( c <- Coffees ) yield c.name
    ;{
      val leakedSession = db.withSession{
        session : Session =>
        query.list()(session)
        session
      }
      assertSQLException{
        query.list()(leakedSession)
      }
    }
    ;{
      val leakedSession = db.withSession{
        session : Session =>
        if( false ){
          query.list()(session)
        }
        session
      }
      assertSQLException{
        query.list()(leakedSession)
      }
    }
  }

  def assertSQLException[T]( f: => T ){
    try{
      f
      fail("leaked session")
    } catch {
      case e:java.sql.SQLException =>
    }
  } 

}
