package scala.slick.test.model

import org.junit.Test
import org.junit.Assert._
import scala.slick.model._
import com.typesafe.slick.testkit.util.{JdbcTestDB, TestkitTest}
import scala.slick.test.model.codegen.roundtrip.Tables
import scala.slick.jdbc.StaticQuery
import scala.slick.testutil._
import scala.slick.testutil.TestDBs._
import com.typesafe.slick.testkit.util.JdbcTestDB

object CodeGeneratorRoundTripTest extends DBTestObject(H2Mem, SQLiteMem, Postgres, MySQL, DerbyMem, HsqldbMem)
class CodeGeneratorRoundTripTest(val tdb: JdbcTestDB) extends DBTest {
  import tdb.profile.simple._
  import tdb.driver.quoteIdentifier
  @Test def test { db.withSession{ implicit session =>
    object Tables extends{
      val profile = tdb.profile
    } with Tables
    import Tables.profile.simple._
    import Tables._
    val ddl = Posts.ddl ++ Categories.ddl ++ TypeTest.ddl ++ X.ddl
    //println(ddl.createStatements.mkString("\n"))
    try{
      ddl.create
    } catch {
      case e:java.sql.SQLSyntaxErrorException => println(ddl.createStatements.mkString("\n")); throw e
    }
    Categories.insert( CategoriesRow(1,"cat") )
    Posts.insertAll(
      PostsRow(1,"post 1",Some(1)),
      PostsRow(2,"post 2",Some(1)),
      PostsRow(3,"post 3",Some(1))
    )
    Categories.insert( CategoriesRow(2,"cat") )
    val res = ( Posts.length.run, Posts.filter(_.title =!= "post 1").map(_.title).run.toList )

    assertEquals( PostsRow(2,"post 2",Some(1)), StaticQuery.queryNA[PostsRow]("select * from "+quoteIdentifier("POSTS")+" where "+quoteIdentifier("id")+" = 2").first )
    assertEquals( PostsRow(2,"post 2",Some(1)), StaticQuery.queryNA("select * from "+quoteIdentifier("POSTS")+" where "+quoteIdentifier("id")+" = 2")(GetResultPostsRow).first )

    assertEquals((3,List("post 2","post 3")), res)

    // Testing table larger 22 columns
    import scala.slick.collection.heterogenous._
    import scala.slick.collection.heterogenous.syntax._

    val oData = 0 :: 11 :: 12 :: 13 :: 14 :: 15 :: 16 :: 21 :: 22 :: 23 :: 24 :: 25 :: 26 :: 31 :: 32 :: 33 :: 34 :: 35 :: 36 :: 41 :: 42 :: 43 :: 44 :: 45 :: 46 :: HNil
    Large.ddl.create
    Large.insert(oData)
    assertEquals((oData,0), Large.map(r => (r,r.id)).first)
    assertEquals(oData, StaticQuery.queryNA[LargeRow]("select * from "+quoteIdentifier("LARGE")).first )
    assertEquals(oData, StaticQuery.queryNA("select * from "+quoteIdentifier("LARGE"))(GetResultLargeRow).first )


    X.map(r => (r.pk,r.pk2,r.column,r.schemaNameXX,r.schemaNameX)).insert((1,1,1,1.1,"test"))

    {
      // testing name and types especially in case of collisions
      import scala.slick.lifted._
      X.map(r =>{(r.pk: Column[Int]) == null})
      X.map(r =>{(r.pk2: Column[Int]) == null})
      X.map(r =>{(r.`val`: Column[Option[Int]]) == null})
      X.map(r =>{(r.column: Column[Int]) == null})
      X.map(r =>{(r.schemaNameXX: Column[Double]) == null})
      X.map(r =>{(r.schemaNameX: Column[String]) == null})
      X.map(r =>{(r.index1: Column[Option[Int]]) == null})
      X.map(r =>{(r.posts: Column[Option[Int]]) == null})
      X.map(r =>{(r.pkX: PrimaryKey) == null})
      X.map(r =>{(r.postsFk: ForeignKeyQuery[Posts,PostsRow]) == null})
      X.map(r =>{(r.categoriesFk2: ForeignKeyQuery[Categories,CategoriesRow]) == null})
      X.map(r =>{(r.categoriesFk3: ForeignKeyQuery[Categories,CategoriesRow]) == null})
      X.map(r =>{(r.index1X: Index) == null})
      X.map(r =>{(r.index2: Index) == null})

      TypeTest.map(r =>{(r.pk: PrimaryKey) == null})
    }
  }}
}
