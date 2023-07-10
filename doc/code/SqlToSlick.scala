package com.typesafe.slick.docs

import scala.concurrent.Await
import scala.concurrent.duration.Duration
import slick.jdbc.H2Profile.api._


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

  lazy val inserts = DBIO.seq(
    addresses += (0,"station 14","Lausanne"),
    addresses += (0,"Grand Central 1","New York City"),
    people += (0,"C. Vogt",999,1),
    people += (0,"J. Vogt",1001,1),
    people += (0,"J. Doe",18,2)
  )

  val db = Database.forConfig("h2mem1")
  try {

    Await.result(db.run(DBIO.seq(
      addresses.schema.create,
      people.schema.create,
      inserts
    )), Duration.Inf)

    def _jdbc = {
      //#jdbc
      import java.sql._

      Class.forName("org.h2.Driver")
      val conn = DriverManager.getConnection("jdbc:h2:mem:test1")
      val people = new scala.collection.mutable.ListBuffer[(Int,String,Int)]()
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
      import slick.jdbc.H2Profile.api._

      //#SlickPlainSQL
      /*
      //#SlickPlainSQL
      val db = Database.forConfig("h2mem1")
      //#SlickPlainSQL
      */
      //#SlickPlainSQL

      val action = sql"select ID, NAME, AGE from PERSON".as[(Int,String,Int)]
      db.run(action)
      //#SlickPlainSQL
    }
    val slickTypesafeQuery = {
      //#SlickTypesafeQuery
      import slick.jdbc.H2Profile.api._

      //#SlickTypesafeQuery
      /*
      //#SlickTypesafeQuery
      val db = Database.forConfig("h2mem1")
      //#SlickTypesafeQuery
      */
      //#SlickTypesafeQuery

      val query = people.map(p => (p.id,p.name,p.age))
      db.run(query.result)
      //#SlickTypesafeQuery
    }
    val tRes = Await.result(slickTypesafeQuery, Duration.Inf)
    val pRes = Await.result(slickPlainSql, Duration.Inf)
    assert(tRes == pRes)
    assert(pRes.size > 0)

    ;{
      import slick.jdbc.H2Profile.api._

      //#slickFunction
      implicit class MyStringColumnExtensions(i: Rep[Int]){
        def squared = i * i
      }
      //#slickFunction
      val squared =
      //#slickFunction

      // usage:
      people.map(p => p.age.squared)
      //#slickFunction
      //#dbFunction
      val power = SimpleFunction.binary[Int,Int,Int]("POWER")
      //#dbFunction
      val pow =
      //#dbFunction

      // usage:
      people.map(p => power(p.age,2))
      //#dbFunction

      val (sRes, pRes) = Await.result(db.run(squared.to[Set].result.zip(pow.to[Set].result)), Duration.Inf)
      assert(sRes == pRes)
      assert(Set(998001,1002001) subsetOf pRes)
    }
    ;{
      //#overrideSql
      people.map(p => (p.id,p.name,p.age))
            .result
            // inject hand-written SQL, see https://gist.github.com/cvogt/d9049c63fc395654c4b4
      //#overrideSql
      /*
      //#overrideSql
            .overrideSql("SELECT id, name, age FROM Person")
      //#overrideSql
      */

      ;{
        val sql =
          //#sqlQueryProjectionWildcard
          sql"select * from PERSON".as[Person]
          //#sqlQueryProjectionWildcard
        val slick =
          //#slickQueryProjectionWildcard
          people.result
          //#slickQueryProjectionWildcard
        val (sqlRes, slickRes) = Await.result(db.run(sql zip slick), Duration.Inf)
        assert(sqlRes == slickRes)
        assert(sqlRes.size > 0)
      };{
        val sql =
          //#sqlQueryProjection
          sql"""
            select AGE, concat(concat(concat(NAME,' ('),ID),')')
            from PERSON
          """.as[(Int,String)]
          //#sqlQueryProjection
        val slick =
          //#slickQueryProjection
          people.map(p => (p.age, p.name ++ " (" ++ p.id.asColumnOf[String] ++ ")")).result
          //#slickQueryProjection
        val (sqlRes, slickRes) = Await.result(db.run(sql zip slick), Duration.Inf)
        assert(sqlRes == slickRes)
        assert(sqlRes.size > 0)
      };{
        val sql =
          //#sqlQueryFilter
          sql"select * from PERSON where AGE >= 18 AND NAME = 'C. Vogt'".as[Person]
          //#sqlQueryFilter
        val slick =
          //#slickQueryFilter
          people.filter(p => p.age >= 18 && p.name === "C. Vogt").result
          //#slickQueryFilter
        val (sqlRes, slickRes) = Await.result(db.run(sql zip slick), Duration.Inf)
        assert(sqlRes == slickRes)
        assert(sqlRes.size > 0)
      };{
        val sql =
          //#sqlQueryOrderBy
          sql"select * from PERSON order by AGE asc, NAME".as[Person]
          //#sqlQueryOrderBy
        val slick =
          //#slickQueryOrderBy
          people.sortBy(p => (p.age.asc, p.name)).result
          //#slickQueryOrderBy
        val (sqlRes, slickRes) = Await.result(db.run(sql zip slick), Duration.Inf)
        assert(sqlRes == slickRes)
        assert(sqlRes.size > 0)
      };{
        val sql =
          //#sqlQueryAggregate
          sql"select max(AGE) from PERSON".as[Option[Int]].head
          //#sqlQueryAggregate
        val slick =
          //#slickQueryAggregate
          people.map(_.age).max.result
          //#slickQueryAggregate
        val (sqlRes, slickRes) = Await.result(db.run(sql zip slick), Duration.Inf)
        assert(sqlRes == slickRes)
      };{
        val sql =
          //#sqlQueryGroupBy
          sql"""
            select ADDRESS_ID, AVG(AGE)
            from PERSON
            group by ADDRESS_ID
          """.as[(Int,Option[Int])]
          //#sqlQueryGroupBy
        val slick =
          //#slickQueryGroupBy
          people.groupBy(p => p.addressId)
                 .map{ case (addressId, group) => (addressId, group.map(_.age).avg) }
                 .result
          //#slickQueryGroupBy
        val (sqlRes, slickRes) = Await.result(db.run(sql zip slick), Duration.Inf)
        assert(sqlRes == slickRes)
        assert(sqlRes.size > 0)
        assert(sqlRes.exists(_._2 == Some(1000)))
      };{
        val sql =
          //#sqlQueryHaving
          sql"""
            select ADDRESS_ID
            from PERSON
            group by ADDRESS_ID
            having avg(AGE) > 50
          """.as[Int]
          //#sqlQueryHaving
        val slick =
          //#slickQueryHaving
          people.groupBy(p => p.addressId)
                 .map{ case (addressId, group) => (addressId, group.map(_.age).avg) }
                 .filter{ case (addressId, avgAge) => avgAge > 50 }
                 .map(_._1)
                 .result
          //#slickQueryHaving
        val (sqlRes, slickRes) = Await.result(db.run(sql zip slick), Duration.Inf)
        assert(sqlRes == slickRes)
        assert(sqlRes.size > 0)
      };{
        val sql =
          //#sqlQueryImplicitJoin
          sql"""
            select P.NAME, A.CITY
            from PERSON P, ADDRESS A
            where P.ADDRESS_ID = a.id
          """.as[(String,String)]
          //#sqlQueryImplicitJoin
        val slick =
          //#slickQueryImplicitJoin
          people.flatMap(p =>
            addresses.filter(a => p.addressId === a.id)
                     .map(a => (p.name, a.city))
          ).result

          //#slickQueryImplicitJoin
        val slick2 =
          //#slickQueryImplicitJoin
          // or equivalent for-expression:
          (for(p <- people;
               a <- addresses if p.addressId === a.id
           ) yield (p.name, a.city)
          ).result
          //#slickQueryImplicitJoin
        val ((sqlRes, slickRes), slick2Res) = Await.result(db.run(sql zip slick zip slick2), Duration.Inf)
        assert(sqlRes == slickRes)
        assert(slickRes == slick2Res)
        assert(sqlRes.size > 0)
      };{
        val sql =
          //#sqlQueryExplicitJoin
          sql"""
            select P.NAME, A.CITY
            from PERSON P
            join ADDRESS A on P.ADDRESS_ID = a.id
          """.as[(String,String)]
          //#sqlQueryExplicitJoin
        val slick =
          //#slickQueryExplicitJoin
          (people join addresses on (_.addressId === _.id))
            .map{ case (p, a) => (p.name, a.city) }.result
          //#slickQueryExplicitJoin
        val (sqlRes, slickRes) = Await.result(db.run(sql zip slick), Duration.Inf)
        assert(sqlRes == slickRes)
        assert(sqlRes.size > 0)
      };{
        val sql =
          //#sqlQueryLeftJoin
          sql"""
            select P.NAME,A.CITY
            from ADDRESS A
            left join PERSON P on P.ADDRESS_ID = a.id
          """.as[(Option[String],String)]
          //#sqlQueryLeftJoin
        val slick =
          //#slickQueryLeftJoin
          (addresses joinLeft people on (_.id === _.addressId))
            .map{ case (a, p) => (p.map(_.name), a.city) }.result
          //#slickQueryLeftJoin
        val (sqlRes, slickRes) = Await.result(db.run(sql zip slick), Duration.Inf)
        assert(sqlRes == slickRes)
        assert(sqlRes.size > 0)
      };{
        val sql =
          //#sqlQueryCollectionSubQuery
          sql"""
            select *
            from PERSON P
            where P.ID in (select ID
                           from ADDRESS
                           where CITY = 'New York City')
          """.as[Person]
          //#sqlQueryCollectionSubQuery
        val slick = {
          //#slickQueryCollectionSubQuery
          val address_ids = addresses.filter(_.city === "New York City").map(_.id)
          people.filter(_.id in address_ids).result // <- run as one query
          //#slickQueryCollectionSubQuery
        }
        val (sqlRes, slickRes) = Await.result(db.run(sql zip slick), Duration.Inf)
        assert(sqlRes == slickRes)
        assert(sqlRes.size > 0)
      };{
        val sql = {
          //#sqlQueryInSet
          val (i1,i2) = (1,2)
          sql"""
            select *
            from PERSON P
            where P.ID in ($i1,$i2)
          """.as[Person]
          //#sqlQueryInSet
        }
        val slick = {
          //#slickQueryInSet
          people.filter(_.id inSet Set(1,2))
                 .result
          //#slickQueryInSet
        }
        val (sqlRes, slickRes) = Await.result(db.run(sql zip slick), Duration.Inf)
        assert(sqlRes == slickRes)
        assert(sqlRes.size > 0)
      };{
        val sql =
          //#sqlQuerySemiRandomChoose
          sql"""
            select * from PERSON P,
                               (select rand() * MAX(ID) as ID from PERSON) RAND_ID
            where P.ID >= RAND_ID.ID
            order by P.ID asc
            limit 1
          """.as[Person].head
          //#sqlQuerySemiRandomChoose
        val slick = {
          //#slickQuerySemiRandomChoose
          val rand = SimpleFunction.nullary[Double]("RAND")

          val rndId = (people.map(_.id).max.asColumnOf[Double] * rand).asColumnOf[Int]

          people.filter(_.id >= rndId)
                 .sortBy(_.id)
                 .result.head
          //#slickQuerySemiRandomChoose
        }
      };{
        val sqlInsert =
          //#sqlQueryInsert
          sqlu"""
            insert into PERSON (NAME, AGE, ADDRESS_ID) values ('M Odersky', 12345, 1)
          """
          //#sqlQueryInsert
        val sqlUpdate =
          //#sqlQueryUpdate
          sqlu"""
            update PERSON set NAME='M. Odersky', AGE=54321 where NAME='M Odersky'
          """
          //#sqlQueryUpdate
        val sqlDelete =
          //#sqlQueryDelete
          sqlu"""
            delete PERSON where NAME='M. Odersky'
          """
          //#sqlQueryDelete

        val slickInsert = {
          //#slickQueryInsert
          people.map(p => (p.name, p.age, p.addressId)) += ("M Odersky",12345,1)
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
        val (sqlInsertRes, slickInsertRes) = Await.result(db.run(sqlInsert zip slickInsert), Duration.Inf)
        assert(sqlInsertRes == slickInsertRes)
        val (sqlUpdateRes, slickUpdateRes) = Await.result(db.run(sqlUpdate zip slickUpdate), Duration.Inf)
        assert(sqlUpdateRes == 2)
        assert(slickUpdateRes == 0)
        val (sqlDeleteRes, slickDeleteRes) = Await.result(db.run(sqlDelete zip slickDelete), Duration.Inf)
        assert(sqlDeleteRes == 2)
        assert(slickDeleteRes == 0)
      };{
        val sql =
          //#sqlCase
          sql"""
            select
              case
                when ADDRESS_ID = 1 then 'A'
                when ADDRESS_ID = 2 then 'B'
              end
            from PERSON P
          """.as[Option[String]]
          //#sqlCase
        val slick =
          //#slickCase
          people.map(p =>
            Case
              If(p.addressId === 1) Then "A"
              If(p.addressId === 2) Then "B"
          ).result
          //#slickCase
        val (sqlRes, slickRes) = Await.result(db.run(sql zip slick), Duration.Inf)
        assert(sqlRes == slickRes)
      }
    }
  } finally db.close
}
