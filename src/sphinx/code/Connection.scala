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
      session : Session =>
      query.list()( session )
    }
    //#withSession
  };{
    //#withSession-implicit
    val query = for( c <- Coffees ) yield c.name
    val result = db.withSession {
      implicit session : Session =>
      query.list // <- takes session implicitly
    }
    // query.list // <- would not compile, no implicit value of type Session
    //#withSession-implicit
  }
  //#independentTransaction
  db.withTransaction{
    implicit session : Session =>
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
      implicit session : Session =>
      (new Helpers).execute(query)
    }
    // (new Helpers).execute(query) // <- Would not compile here (no implicit session)
    //#helpers
  }
  ;{
    //Coffees.ddl.create(session)
    //#threadLocalSession
    //#threadLocalSession-import
    import Database.threadLocalSession // <- implicit def threadLocalSession : Session
    //#threadLocalSession-import
    object helpers{
      def execute[T]( query:Query[T,_] ) = query.list // uses threadLocalSession to try to get the Session
    }
    val query = for( c <- Coffees ) yield c.name
    db.withSession { // <- creates a Session and stores it as thread local
      helpers.execute(query)
    }
    try{
      helpers.execute(query) // <- leads to an exception, because execute requires an available session
    }catch{case e:SlickException => }
    //#threadLocalSession
  }
  ;{
    //#withSession-empty
    db.withSession {
      // your queries go here
    }
    //#withSession-empty
  }
  ;{
    val query = for( c <- Coffees ) yield c.name

    val leakedSession = db.withSession {
      implicit session : Session =>
      session
    }
    var r : List[_] = null
    try{
      r = query.list()(leakedSession)
      throw new SomeException("failed: "+r)
    } catch {
      case e:SomeException => throw e
      case _:Exception =>
    }
    
    val closure = db.withSession {
      implicit session : Session =>
      (x:Unit) => query.list
    }

    try{
      r = closure()
      throw new SomeException("failed: "+r)
    } catch {
      case e:SomeException => throw e
      case _:Exception =>
    }

    import scala.concurrent._
    import scala.util._
    import ExecutionContext.Implicits.global
    db.withSession {
      implicit session : Session =>
      future{
        Thread.sleep(1000)
        query.list
      }
    }.onComplete {
      case Success(r) => throw new Exception("failed: "+r)
      case Failure(_) =>
    }
  }
  ;{
    trait Parent{
      def id : 
    }
    object Coffees extends Table[String]("COFFEES") {
      def id = column[String]("COF_NAME", O.PrimaryKey)
      def * = name
    }
  }
}
