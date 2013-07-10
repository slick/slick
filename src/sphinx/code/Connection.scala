package com.typesafe.slick.docs
import scala.slick.driver.H2Driver.simple._

/**
 * A simple example that uses statically typed queries against an in-memory
 * H2 database. The example data comes from Oracle's JDBC tutorial at
 * http://download.oracle.com/javase/tutorial/jdbc/basics/tables.html.
 */
object Connection extends App {
  object Coffees extends Table[String]("COFFEES") {
    def name = column[String]("COF_NAME", O.PrimaryKey)
    def * = name
  }
  if(false){
    val dataSource = null.asInstanceOf[javax.sql.DataSource]
    //#forDataSource
    val db = Database.forDataSource( dataSource : javax.sql.DataSource )
    //#forDataSource
  }
  if(false){ 
    val JNDIName = ""
    //#forName
    val db = Database.forName( JNDIName : String )
    //#forName
  }
  ;{
    //#forURL
    val db = Database.forURL("jdbc:h2:mem:test1;DB_CLOSE_DELAY=-1", driver = "org.h2.Driver")
    //#forURL
  }
  val db = Database.forURL("jdbc:h2:mem:test2;INIT="+Coffees.ddl.createStatements.mkString("\\;"), driver = "org.h2.Driver")
  ;{
    //#withSession
    val query = for( c <- Coffees ) yield c.name
    val result = db.withSession {
      session =>
      query.list()( session )
    }
    //#withSession
  };{
    //#withSession-implicit
    val query = for( c <- Coffees ) yield c.name
    val result = db.withSession {
      implicit session =>
      query.list // <- takes session implicitly
    }
    // query.list // <- would not compile, no implicit value of type Session
    //#withSession-implicit
  }
  //#independentTransaction
  db.withTransaction{
    implicit session =>
    // your queries go here
  }
  class SomeException(s:String) extends Exception(s)
  //#independentTransaction
  db.withSession {
    session : Session =>
    //#transaction
    session.withTransaction {
      try{
        // your queries go here
      } catch{
        case e:SomeException =>
          session.rollback // signals to rollback when leaving scope
          // handle exception here
      }
    }
    //#transaction
  }
  ;{
    //#manual-session
    val query = for( c <- Coffees ) yield c.name
    val session : Session = db.createSession
    val result  = query.list()( session )
    session.close
    //#manual-session
  }
  ;{
    //#helpers
    class Helpers( implicit session:Session ){
      def execute[T]( query:Query[T,_] ) = query.list
      // ... place futher helpers methods here
    }
    val query = for( c <- Coffees ) yield c.name
    db.withSession {
      implicit session =>
      (new Helpers).execute(query)
    }
    // (new Helpers).execute(query) // <- Would not compile here (no implicit session)
    //#helpers
  }
  ;{
    //Coffees.ddl.create(session)
    //#dynamicSession
    //#dynamicSession-import
    import Database.dynamicSession // <- implicit def dynamicSession : Session
    //#dynamicSession-import
    object helpers{
      def execute[T]( query:Query[T,_] ) = query.list // uses dynamicSession to try to get the Session
    }
    val query = for( c <- Coffees ) yield c.name
    db.withDynSession { // <- creates a Session and stores it as dynamicSession
      helpers.execute(query)
    }
    try{
      helpers.execute(query) // <- leads to an exception, because execute requires an available session
    }catch{case e:SlickException => }
    //#dynamicSession
  }
  ;{
    //#withSession-empty
    db.withDynSession {
      // your queries go here
    }
    //#withSession-empty
  }
}
