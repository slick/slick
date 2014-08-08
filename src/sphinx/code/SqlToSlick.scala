package com.typesafe.slick.docs
import scala.slick.driver.H2Driver.simple._

/**
 * A simple example that uses statically typed queries against an in-memory
 * H2 database. The example data comes from Oracle's JDBC tutorial at
 * http://download.oracle.com/javase/tutorial/jdbc/basics/tables.html.
 */
object SqlToSlick extends App {

  object Tables{
    //#tableClasses
    type Person = (Int,String,String,Int,Int)
    class Persons(tag: Tag) extends Table[Person](tag, "PERSON") {
      def id = column[Int]("ID", O.PrimaryKey, O.AutoInc)
      def first = column[String]("FIRST")
      def last = column[String]("LAST")
      def age = column[Int]("AGE")
      def livesAt = column[Int]("LIVES_AT")
      def * = (id,first,last,age,livesAt)
      def livesAtFK = foreignKey("lives_at_fk",livesAt,addresses)(_.id)
    }
    lazy val persons = TableQuery[Persons]

    type Address = (Int,String,String)
    class Addresses(tag: Tag) extends Table[Address](tag, "ADDRESS") {
      def id = column[Int]("ID", O.PrimaryKey, O.AutoInc)
      def street = column[String]("STREET")
      def city = column[String]("CITY")
      def * = (id,street,city)
    }
    lazy val addresses = TableQuery[Addresses]
    //#tableClasses
  }
  import Tables._

  val jdbcDriver = "org.h2.Driver"  
  val dbUrl = "jdbc:h2:mem:sqltoslick;DB_CLOSE_DELAY=-1"

  Database.forURL(dbUrl,driver=jdbcDriver) withSession {
    implicit session =>
    addresses.ddl.create
    addresses.insert(0,"station 14","Lausanne")
    addresses.insert(0,"Broadway 1","New York City")

    persons.ddl.create
    persons.insert((0,"Chris","Vogt",999,1))
    persons.insert((0,"John","Vogt",1001,1))
    persons.insert((0,"John","Doe",18,2))
  }

  val jdbc = {
    //#jdbc
    import java.sql._

    Class.forName(jdbcDriver)
    val conn = DriverManager.getConnection(dbUrl)
    val people = new scala.collection.mutable.MutableList[(Int,String,String,Int)]()
    try{
      val stmt = conn.createStatement()
      try{

        val rs = stmt.executeQuery("select ID, FIRST, LAST, AGE from PERSON")
        try{
          while(rs.next()){
            people += ((rs.getInt(1), rs.getString(2), rs.getString(3), rs.getInt(4)))
          }
        }finally{
          rs.close()
        }

      }finally{
        stmt.close()
      }
    }finally{
      conn.close()
    }
    //#jdbc
    people
  }
  val slickPlainSql = {
    //#SlickPlainSQL
    import scala.slick.driver.H2Driver.simple._
    import scala.slick.jdbc.StaticQuery.interpolation
    import scala.slick.jdbc.GetResult
    
    val db = Database.forURL(dbUrl,driver=jdbcDriver)
    
    val query = sql"select ID, FIRST, LAST, AGE from PERSON".as[(Int,String,String,Int)]
    val people = db.withSession{ implicit session =>
      query.list
    }
    //#SlickPlainSQL
    people
  }
  val slickTypesafeQuery = {
    import Tables.Persons // <- import auto-generated or hand-written TableQuery
    //#SlickTypesafeQuery
    import scala.slick.driver.H2Driver.simple._

    val db = Database.forURL(dbUrl,driver=jdbcDriver)

    val query = persons.map(p => (p.id,p.first,p.last,p.age))
    val people = db.withSession{ implicit session =>
      query.run
    }
    //#SlickTypesafeQuery
    people
  }
  assert(jdbc == slickPlainSql)
  assert(slickTypesafeQuery == slickPlainSql,(slickTypesafeQuery, slickPlainSql).toString)
  assert(jdbc.size > 0)

  ;{
    import scala.slick.driver.H2Driver.simple._
    import scala.slick.jdbc.StaticQuery.interpolation
    import scala.slick.jdbc.GetResult
    import Tables.Persons // <- import auto-generated or hand-written TableQuery
    val db = Database.forURL(dbUrl,driver=jdbcDriver)

    db.withSession{ implicit session =>
      //#slickFunction
      implicit class MyStringColumnExtensions(i: Column[Int]){
        def squared = i * i
      }
      //#slickFunction
      val squared = 
      //#slickFunction

      // usage:
      persons.map(p => p.age.squared)
      //#slickFunction
      .run
      //#dbFunction
      val power = SimpleFunction.binary[Int,Int,Int]("POWER")
      //#dbFunction
      val pow =
      //#dbFunction

      // usage:
      persons.map(p => power(p.age,2))
      //#dbFunction
      .run

      assert(squared.toSet == pow.toSet)
      assert(Set(998001,1002001) subsetOf pow.toSet)
    }
  }
  ;{
    Database.forURL(dbUrl,driver=jdbcDriver) withSession { implicit session =>
      //#overrideSql
      persons.map(p => (p.id,p.first,p.last,p.age))
             // inject hand-written SQL, see https://gist.github.com/cvogt/d9049c63fc395654c4b4
      //#overrideSql
      /*
      //#overrideSql
             .overrideSql("SELECT id, first, last, age FROM Person")
      //#overrideSql
      */
      //#overrideSql
             .list
      //#overrideSql

      import scala.slick.jdbc.StaticQuery.interpolation
      ;{
        val sql =
          //#sqlQueryProjection*
          sql"select * from PERSON".as[Person].list
          //#sqlQueryProjection*
        val slick =
          //#slickQueryProjection*
          persons.run
          //#slickQueryProjection*
        assert(sql == slick,(sql,slick).toString)
        assert(sql.size > 0)
      };{
        val sql =
          //#sqlQueryProjection
          sql"""
            select AGE, concat(concat(FIRST,' '),LAST)
            from PERSON
          """.as[(Int,String)].list
          //#sqlQueryProjection
        val slick =
          //#slickQueryProjection
          persons.map(p => (p.age, p.first ++ " " ++ p.last)).run
          //#slickQueryProjection
        assert(sql == slick,(sql,slick).toString)
        assert(sql.size > 0)
      };{
        val sql =
          //#sqlQueryFilter
          sql"select * from PERSON where AGE >= 18 AND LAST = 'Vogt'".as[Person].list
          //#sqlQueryFilter
        val slick =
          //#slickQueryFilter
          persons.filter(p => p.age >= 18 && p.last === "Vogt").run
          //#slickQueryFilter
        assert(sql == slick,(sql,slick).toString)
        assert(sql.size > 0)
      };{
        val sql =
          //#sqlQueryOrderBy
          sql"select * from PERSON order by LAST asc, FIRST".as[Person].list
          //#sqlQueryOrderBy
        val slick =
          //#slickQueryOrderBy
          persons.sortBy(p => (p.last.asc, p.first)).run
          //#slickQueryOrderBy
        assert(sql == slick,(sql,slick).toString)
        assert(sql.size > 0)
      };{
        val sql =
          //#sqlQueryAggregate
          sql"select max(AGE) from PERSON".as[Option[Int]].first
          //#sqlQueryAggregate
        val slick =
          //#slickQueryAggregate
          persons.map(_.age).max.run
          //#slickQueryAggregate
        assert(sql == slick, (sql,slick).toString)
      };{
        val sql =
          //#sqlQueryGroupBy
          sql"""
            select LAST, avg(AGE)
            from PERSON
            group by LAST
          """.as[(String,Option[Int])].list
          //#sqlQueryGroupBy
        val slick =
          //#slickQueryGroupBy
          persons.groupBy(p => p.last)
                 .map{ case (last, group) => (last, group.map(_.age).avg) }
                 .run
          //#slickQueryGroupBy
        assert(sql == slick,(sql,slick).toString)
        assert(sql.size > 0)

        assert(sql == slick,(sql,slick).toString )
        assert(sql.exists(_._2 == Some(1000)))
      };{
        val sql =
          //#sqlQueryHaving
          sql"""
            select LAST
            from PERSON
            group by LAST
            having avg(AGE) > 50
          """.as[String].list
          //#sqlQueryHaving
        val slick =
          //#slickQueryHaving
          persons.groupBy(p => p.last)
                 .map{ case (last, group) => (last, group.map(_.age).avg) }
                 .filter{ case (last, avgAge) => avgAge > 50 }
                 .map(_._1)
                 .run
          //#slickQueryHaving
        assert(sql == slick,(sql,slick).toString)
        assert(sql.size > 0)
      };{
        val sql =
          //#sqlQueryImplicitJoin
          sql"""
            select P.LAST, A.CITY
            from PERSON P, ADDRESS A
            where P.LIVES_AT = a.id
          """.as[(String,String)].list
          //#sqlQueryImplicitJoin
        val slick =
          //#slickQueryImplicitJoin
          persons.flatMap(p =>
            addresses.filter(a => p.livesAt === a.id)
                     .map(a => (p.last, a.city))
          ).run
          
          //#slickQueryImplicitJoin
        val slick2 =
          //#slickQueryImplicitJoin
          // or equivalent for-expression:
          (for(p <- persons;
               a <- addresses if p.livesAt === a.id
           ) yield (p.last, a.city)
          ).run
          //#slickQueryImplicitJoin
        assert(sql == slick,(sql,slick).toString)
        assert(slick2 == slick)
        assert(sql.size > 0)
      };{
        val sql =
          //#sqlQueryExplicitJoin
          sql"""
            select P.LAST, A.CITY
            from PERSON P
            join ADDRESS A on P.LIVES_AT = a.id
          """.as[(String,String)].list
          //#sqlQueryExplicitJoin
        val slick =
          //#slickQueryExplicitJoin
          (persons join addresses on (_.livesAt === _.id))
            .map{ case (p, a) => (p.last, a.city) }.run
          //#slickQueryExplicitJoin
        assert(sql == slick,(sql,slick).toString)
        assert(sql.size > 0)
      };{
        val sql =
          //#sqlQueryLeftJoin
          sql"""
            select P.LAST,A.CITY
            from ADDRESS A
            left join PERSON P on P.LIVES_AT = a.id
          """.as[(Option[String],String)].list
          //#sqlQueryLeftJoin
        val slick =
          //#slickQueryLeftJoin
          (addresses leftJoin persons on (_.id === _.livesAt))
            .map{ case (a, p) => (p.last.?, a.city) }.run
          //#slickQueryLeftJoin
        assert(sql == slick,(sql,slick).toString)
        assert(sql.size > 0)
      };{
        val sql =
          //#sqlQueryCollectionSubQuery
          sql"""
            select *
            from PERSON P
            where P.ID in (select ID
                           from ADDRESS
                           where CITY = 'New York City')
          """.as[Person].list
          //#sqlQueryCollectionSubQuery
        val slick = {
          //#slickQueryCollectionSubQuery
          val address_ids = addresses.filter(_.city === "New York City").map(_.id)
          persons.filter(_.id in address_ids).run // <- run as one query
          //#slickQueryCollectionSubQuery
        }
        assert(sql == slick,(sql,slick).toString)
        assert(sql.size > 0)
      };{
        val sql = {
          //#sqlQueryInSet
          val (i1,i2) = (1,2)
          sql"""
            select *
            from PERSON P
            where P.ID in ($i1,$i2)
          """.as[Person].list
          //#sqlQueryInSet
        }
        val slick = {
          //#slickQueryInSet
          persons.filter(_.id inSet Set(1,2))
                 .run
          //#slickQueryInSet
        }  
        assert(sql == slick,(sql,slick).toString)
        assert(sql.size > 0)
      };{
        val sql =
          //#sqlQuerySemiRandomChoose
          sql"""
            select * from PERSON P,
                               (select rand() * MAX(ID) as ID from PERSON) RAND_ID
            where P.ID >= RAND_ID.ID
            order by P.ID asc
            limit 1
          """.as[Person].first
          //#sqlQuerySemiRandomChoose
        val slick = {
          //#slickQuerySemiRandomChoose
          val rand = SimpleFunction.nullary[Double]("RAND")
          
          val rndId = (persons.map(_.id).max.asColumnOf[Double] * rand).asColumnOf[Int]

          persons.filter(_.id >= rndId)
                 .sortBy(_.id)
                 .first
          //#slickQuerySemiRandomChoose
        }
      };{
        val sqlInsert =
          //#sqlQueryInsert
          sqlu"""
            insert into PERSON (FIRST, LAST, AGE, LIVES_AT) values ('Stefan', 'Geiger', 12345, 1)
          """.first
          //#sqlQueryInsert
        val sqlUpdate =
          //#sqlQueryUpdate
          sqlu"""
            update PERSON set LAST='Zeiger', AGE=54321
          """.first
          //#sqlQueryUpdate
        val sqlDelete =
          //#sqlQueryDelete
          sqlu"""
            delete PERSON where FIRST='Stefan' and LAST='Zeiger'
          """.first
          //#sqlQueryDelete

        val slickInsert = {
          //#slickQueryInsert
          persons.map(p => (p.first, p.last, p.age, p.livesAt))
                 .insert(("Stefan","Zeiger",12345,1))
          //#slickQueryInsert
        }
        val slickUpdate = {
          //#slickQueryUpdate
          persons.map(p => (p.last,p.age))
                 .update(("Zeiger",54321))
          //#slickQueryUpdate
        }
        val slickDelete = {
          //#slickQueryDelete
          persons.filter(p => p.first === "Stefan" && p.last === "Zeiger")
                 .delete
          //#slickQueryDelete
        }
        assert(sqlInsert == slickInsert,(sqlInsert,slickInsert).toString)
        assert(sqlUpdate == slickUpdate,(sqlUpdate,slickUpdate).toString)
        assert(sqlDelete == slickDelete,(sqlDelete,slickDelete).toString)
      }
    }
  }
}
