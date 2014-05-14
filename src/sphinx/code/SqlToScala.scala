package com.typesafe.slick.docs
import scala.slick.driver.H2Driver.simple._

/**
 * A simple example that uses statically typed queries against an in-memory
 * H2 database. The example data comes from Oracle's JDBC tutorial at
 * http://download.oracle.com/javase/tutorial/jdbc/basics/tables.html.
 */
object SqlToScala extends App {

  object Tables{
    class Persons(tag: Tag) extends Table[(Int,String,String,Int)](tag, "PERSON") {
      def id = column[Int]("ID", O.PrimaryKey)
      def first = column[String]("FIRST")
      def last = column[String]("LAST")
      def age = column[Int]("AGE")
      def * = (id,first,last,age)
    }
    val Persons = TableQuery[Persons]    
  }
  import Tables._

  val JDBC_DRIVER = "org.h2.Driver";  
  val DB_URL = "jdbc:h2:mem:sqltoslick;DB_CLOSE_DELAY=-1";

  Database.forURL(DB_URL,driver=JDBC_DRIVER) withSession {
    implicit session =>
    Persons.ddl.create
    Persons.insert((1,"Chris","Vogt",999))
    Persons.insert((2,"John","Vogt",1001))
  }

  val jdbc = {
    //#jdbc
    
    import java.sql._;

    Class.forName(JDBC_DRIVER);
    val conn = DriverManager.getConnection(DB_URL);
    val stmt = conn.createStatement();

    val rs = stmt.executeQuery("SELECT id, first, last, age FROM Person");
    val people = new scala.collection.mutable.MutableList[(Int,String,String,Int)]()
    while(rs.next()){
      people += ((rs.getInt(1), rs.getString(2), rs.getString(3), rs.getInt(4)))
    }
    rs.close();
    stmt.close();
    conn.close();
    //#jdbc
    people
  }
  val slickPlainSql = {
    //#SlickPlainSQL
    import scala.slick.driver.H2Driver.simple._
    import scala.slick.jdbc.StaticQuery.interpolation
    import scala.slick.jdbc.GetResult
    
    val db = Database.forURL(DB_URL,driver=JDBC_DRIVER)
    
    val query = sql"SELECT id, first, last, age FROM Person".as[(Int,String,String,Int)]
    val people = db.withSession{ implicit session =>
      query.list
    }
    //#SlickPlainSQL
    people
  }
  val slickTypesafeQuery = {
    //#SlickTypesafeQuery
    import scala.slick.driver.H2Driver.simple._
    import Tables.Persons // <- import auto-generated or hand-written TableQuery

    val db = Database.forURL(DB_URL,driver=JDBC_DRIVER)

    val query = Persons.map(p => (p.id,p.first,p.last,p.age))
    val people = db.withSession{ implicit session =>
      query.run
    }
    //#SlickTypesafeQuery
    people
  }
  assert(jdbc == slickPlainSql)
  assert(slickTypesafeQuery == slickPlainSql)
  assert(jdbc.size == 2)

  ;{
    import scala.slick.driver.H2Driver.simple._
    import scala.slick.jdbc.StaticQuery.interpolation
    import scala.slick.jdbc.GetResult
    import Tables.Persons // <- import auto-generated or hand-written TableQuery
    val db = Database.forURL(DB_URL,driver=JDBC_DRIVER)

    val groupBy = {
      db.withSession{ implicit session =>
        val sqlGroupBy =
          //#groupBySQL
          sql"select avg(AGE), LAST from PERSON group by last".as[(Int,String)]
          //#groupBySQL

        val slickGroupBy =
          //#groupBySlick
          Persons.groupBy(_.last).map{ case (last,group) => (group.map(_.age).avg, last) }
          //#groupBySlick

        //throw new Exception((sqlGroupBy.list, slickGroupBy.run).toString)
        assert(sqlGroupBy.list == (slickGroupBy.run.map{case (Some(age),last) => (age,last); case _ => ???}),(sqlGroupBy.list +" "+ slickGroupBy.run).toString )
        assert(sqlGroupBy.list.size == 1)
        assert(sqlGroupBy.first._1 == 1000)


        //#slickFunction
        implicit class MyStringColumnExtensions(i: Column[Int]){
          def squared = i * i
        }
        //#slickFunction
        val squared = 
        //#slickFunction
        Persons.map(p => p.age.squared)
        //#slickFunction
        .run
        //#dbFunction
        val power = SimpleFunction.binary[Int,Int,Int]("POWER")
        //#dbFunction
        val pow =
        //#dbFunction
        Persons.map(p => power(p.age,2))
        //#dbFunction
        .run

        assert(squared.toSet == pow.toSet)
        assert(pow.toSet == Set(998001,1002001))
      }
    }
  }
  ;{
    //#overrideSql
    import scala.slick.lifted.{TableQuery => _}
    import scala.slick.ast._
    import scala.slick.driver._
    import scala.language.implicitConversions
     
    /** Extends QueryInvoker to allow overriding used SQL statement when executing a query */
    trait OverridingInvoker extends JdbcDriver{
      // get the extended QueryInvoker into the .simple._ implicits
      override val Implicit: Implicits = new Implicits
      override val simple: Implicits with SimpleQL = new Implicits with SimpleQL
      class Implicits extends super.Implicits {
        override implicit def queryToAppliedQueryInvoker[U](q: Query[_,U])
         = super.queryToAppliedQueryInvoker(q: Query[_,U]).asInstanceOf[UnitQueryInvoker[U]]
      }
     
      override def createUnitQueryInvoker[R](tree: Node): UnitQueryInvoker[R] = new UnitQueryInvoker[R](tree)

      // extended QueryInvoker
      class UnitQueryInvoker[R](n: Node) extends super.UnitQueryInvoker[R](n) {
        def overrideSql(sql: String) = new PatchedUnitQueryInvoker[R](sql,n)
      }
     
      // QueryInvokers that patch the used SQL
      class PatchedUnitQueryInvoker[U](sql:String, n: Node) extends UnitQueryInvoker[U](n){
          override def getStatement = sql
      }
    }
     
    // Create a custom MySQL driver patching in the Overriding invoker
    object CustomH2Driver extends H2Driver with OverridingInvoker
     
    // Import stuff from the patched driver instead of the default
    import CustomH2Driver.simple._

    class Persons(tag: Tag) extends Table[(Int,String,String,Int)](tag, "PERSON") {
      def id = column[Int]("ID", O.PrimaryKey)
      def first = column[String]("FIRST")
      def last = column[String]("LAST")
      def age = column[Int]("AGE")
      def * = (id,first,last,age)
    }
    val Persons = TableQuery[Persons] 

    // please fill in jdbc connection url and driver
    Database.forURL(DB_URL,driver=JDBC_DRIVER) withSession { implicit session =>
      println(
        Persons.map(p => (p.id,p.first,p.last,p.age))
               // inject hand-written SQL
               .overrideSql("SELECT id, first, last, age FROM Person")
               .list
      )
    }
    //#overrideSql
  }
}
