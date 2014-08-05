package com.typesafe.slick.docs
import scala.slick.driver.H2Driver.simple._

object SqlToSlick extends App {

  object Tables{
    //#tableClasses
    type Person = (Int,String,Int,Int)
    class People(tag: Tag) extends Table[Person](tag, "PERSON") {
      def id = column[Int]("ID", O.PrimaryKey, O.AutoInc)
      def name = column[String]("NAME")
      def age = column[Int]("AGE")
      def addressId = column[Int]("ADDRESS_ID")
      def * = (id,name,age,addressId)
      def address = foreignKey("ADDRESS",addressId,addresses)(_.id)
    }
    lazy val people = TableQuery[People]

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
  def inserts(implicit session: Session) = {
    addresses.insert(0,"station 14","Lausanne")
    addresses.insert(0,"Grand Central 1","New York City")

    people.insert((0,"C. Vogt",999,1))
    people.insert((0,"J. Vogt",1001,1))
    people.insert((0,"J. Doe",18,2))
  }

  Database.forURL(dbUrl,driver=jdbcDriver) withSession { implicit s =>
    addresses.ddl.create
    people.ddl.create
    inserts
  }

  val jdbc = {
    //#jdbc
    import java.sql._

    Class.forName(jdbcDriver)
    val conn = DriverManager.getConnection(dbUrl)
    val people = new scala.collection.mutable.MutableList[(Int,String,Int)]()
    try{
      val stmt = conn.createStatement()
      try{

        val rs = stmt.executeQuery("select ID, NAME, AGE from PERSON")
        try{
          while(rs.next()){
            people += ((rs.getInt(1), rs.getString(2), rs.getInt(3)))
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
    
    val query = sql"select ID, NAME, AGE from PERSON".as[(Int,String,Int)]
    val people = db.withSession{ implicit session =>
      query.list
    }
    //#SlickPlainSQL
    people
  }
  val slickTypesafeQuery = {
    import Tables.People // <- import auto-generated or hand-written TableQuery
    //#SlickTypesafeQuery
    import scala.slick.driver.H2Driver.simple._

    val db = Database.forURL(dbUrl,driver=jdbcDriver)

    val query = people.map(p => (p.id,p.name,p.age))
    val result = db.withSession{ implicit session =>
      query.run
    }
    //#SlickTypesafeQuery
    result
  }
  assert(jdbc == slickPlainSql)
  assert(slickTypesafeQuery == slickPlainSql,(slickTypesafeQuery, slickPlainSql).toString)
  assert(jdbc.size > 0)

  ;{
    import scala.slick.driver.H2Driver.simple._
    import scala.slick.jdbc.StaticQuery.interpolation
    import scala.slick.jdbc.GetResult
    import Tables.People // <- import auto-generated or hand-written TableQuery
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
      people.map(p => p.age.squared)
      //#slickFunction
      .run
      //#dbFunction
      val power = SimpleFunction.binary[Int,Int,Int]("POWER")
      //#dbFunction
      val pow =
      //#dbFunction

      // usage:
      people.map(p => power(p.age,2))
      //#dbFunction
      .run

      assert(squared.toSet == pow.toSet)
      assert(Set(998001,1002001) subsetOf pow.toSet)
    }
  }
  ;{
    Database.forURL(dbUrl,driver=jdbcDriver) withSession { implicit session =>
      //#overrideSql
      people.map(p => (p.id,p.name,p.age))
             // inject hand-written SQL, see https://gist.github.com/cvogt/d9049c63fc395654c4b4
      //#overrideSql
      /*
      //#overrideSql
             .overrideSql("SELECT id, name, age FROM Person")
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
          people.run
          //#slickQueryProjection*
        assert(sql == slick,(sql,slick).toString)
        assert(sql.size > 0)
      };{
        val sql =
          //#sqlQueryProjection
          sql"""
            select AGE, concat(concat(concat(NAME,' ('),ID),')')
            from PERSON
          """.as[(Int,String)].list
          //#sqlQueryProjection
        val slick =
          //#slickQueryProjection
          people.map(p => (p.age, p.name ++ " (" ++ p.id.asColumnOf[String] ++ ")")).run
          //#slickQueryProjection
        assert(sql == slick,(sql,slick).toString)
        assert(sql.size > 0)
      };{
        val sql =
          //#sqlQueryFilter
          sql"select * from PERSON where AGE >= 18 AND NAME = 'C. Vogt'".as[Person].list
          //#sqlQueryFilter
        val slick =
          //#slickQueryFilter
          people.filter(p => p.age >= 18 && p.name === "C. Vogt").run
          //#slickQueryFilter
        assert(sql == slick,(sql,slick).toString)
        assert(sql.size > 0)
      };{
        val sql =
          //#sqlQueryOrderBy
          sql"select * from PERSON order by AGE asc, NAME".as[Person].list
          //#sqlQueryOrderBy
        val slick =
          //#slickQueryOrderBy
          people.sortBy(p => (p.age.asc, p.name)).run
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
          people.map(_.age).max.run
          //#slickQueryAggregate
        assert(sql == slick, (sql,slick).toString)
      };{
        val sql =
          //#sqlQueryGroupBy
          sql"""
            select ADDRESS_ID, AVG(AGE)
            from PERSON
            group by ADDRESS_ID
          """.as[(Int,Option[Int])].list
          //#sqlQueryGroupBy
        val slick =
          //#slickQueryGroupBy
          people.groupBy(p => p.addressId)
                 .map{ case (addressId, group) => (addressId, group.map(_.age).avg) }
                 .list
          //#slickQueryGroupBy
        assert(sql == slick,(sql,slick).toString)
        assert(sql.size > 0)

        assert(sql == slick,(sql,slick).toString )
        assert(sql.exists(_._2 == Some(1000)))
      };{
        val sql =
          //#sqlQueryHaving
          sql"""
            select ADDRESS_ID
            from PERSON
            group by ADDRESS_ID
            having avg(AGE) > 50
          """.as[Int].list
          //#sqlQueryHaving
        val slick =
          //#slickQueryHaving
          people.groupBy(p => p.addressId)
                 .map{ case (addressId, group) => (addressId, group.map(_.age).avg) }
                 .filter{ case (addressId, avgAge) => avgAge > 50 }
                 .map(_._1)
                 .run
          //#slickQueryHaving
        assert(sql == slick,(sql,slick).toString)
        assert(sql.size > 0)
      };{
        val sql =
          //#sqlQueryImplicitJoin
          sql"""
            select P.NAME, A.CITY
            from PERSON P, ADDRESS A
            where P.ADDRESS_ID = a.id
          """.as[(String,String)].list
          //#sqlQueryImplicitJoin
        val slick =
          //#slickQueryImplicitJoin
          people.flatMap(p =>
            addresses.filter(a => p.addressId === a.id)
                     .map(a => (p.name, a.city))
          ).run
          
          //#slickQueryImplicitJoin
        val slick2 =
          //#slickQueryImplicitJoin
          // or equivalent for-expression:
          (for(p <- people;
               a <- addresses if p.addressId === a.id
           ) yield (p.name, a.city)
          ).run
          //#slickQueryImplicitJoin
        assert(sql == slick,(sql,slick).toString)
        assert(slick2 == slick)
        assert(sql.size > 0)
      };{
        val sql =
          //#sqlQueryExplicitJoin
          sql"""
            select P.NAME, A.CITY
            from PERSON P
            join ADDRESS A on P.ADDRESS_ID = a.id
          """.as[(String,String)].list
          //#sqlQueryExplicitJoin
        val slick =
          //#slickQueryExplicitJoin
          (people join addresses on (_.addressId === _.id))
            .map{ case (p, a) => (p.name, a.city) }.run
          //#slickQueryExplicitJoin
        assert(sql == slick,(sql,slick).toString)
        assert(sql.size > 0)
      };{
        val sql =
          //#sqlQueryLeftJoin
          sql"""
            select P.NAME,A.CITY
            from ADDRESS A
            left join PERSON P on P.ADDRESS_ID = a.id
          """.as[(Option[String],String)].list
          //#sqlQueryLeftJoin
        val slick =
          //#slickQueryLeftJoin
          (addresses leftJoin people on (_.id === _.addressId))
            .map{ case (a, p) => (p.name.?, a.city) }.run
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
          people.filter(_.id in address_ids).run // <- run as one query
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
          people.filter(_.id inSet Set(1,2))
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
          
          val rndId = (people.map(_.id).max.asColumnOf[Double] * rand).asColumnOf[Int]

          people.filter(_.id >= rndId)
                 .sortBy(_.id)
                 .first
          //#slickQuerySemiRandomChoose
        }
      };{
        val sqlInsert =
          //#sqlQueryInsert
          sqlu"""
            insert into PERSON (NAME, AGE, ADDRESS_ID) values ('M Odersky', 12345, 1)
          """.first
          //#sqlQueryInsert
        val sqlUpdate =
          //#sqlQueryUpdate
          sqlu"""
            update PERSON set NAME='M. Odersky', AGE=54321 where NAME='M Odersky'
          """.first
          //#sqlQueryUpdate
        val sqlDelete =
          //#sqlQueryDelete
          sqlu"""
            delete PERSON where NAME='M. Odersky'
          """.first
          //#sqlQueryDelete

        val slickInsert = {
          //#slickQueryInsert
          people.map(p => (p.name, p.age, p.addressId))
                 .insert(("M Odersky",12345,1))
          //#slickQueryInsert
        }
        val slickUpdate = {
          //#slickQueryUpdate
          people.filter(_.name === "M Odersky")
                 .map(p => (p.name,p.age))
                 .update(("M. Odersky",54321))
          //#slickQueryUpdate
        }
        val slickDelete = {
          //#slickQueryDelete
          people.filter(p => p.name === "M. Odersky")
                 .delete
          //#slickQueryDelete
        }
        assert(sqlInsert == slickInsert,(sqlInsert,slickInsert).toString)
        assert(sqlUpdate == slickUpdate,(sqlUpdate,slickUpdate).toString)
        assert(sqlDelete == slickDelete,(sqlDelete,slickDelete).toString)
      };{
        val sqlCase = 
          //#sqlCase
          sql"""
            select
              case 
                when ADDRESS_ID = 1 then 'A'
                when ADDRESS_ID = 2 then 'B'
              end
            from PERSON P
          """.as[Option[String]].list
          //#sqlCase
        val slickCase = 
          //#slickCase
          people.map(p =>
            Case
              If(p.addressId === 1) Then "A"
              If(p.addressId === 2) Then "B"
          ).list
          //#slickCase
        assert(sqlCase == slickCase, (sqlCase,slickCase).toString)
      }
    }
  }
}
