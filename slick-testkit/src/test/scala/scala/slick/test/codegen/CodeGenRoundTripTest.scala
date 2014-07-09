package scala.slick.test.codegen

import org.junit.Test
import org.junit.Assert._
import com.typesafe.slick.testkit.util.TestkitTest
import scala.slick.jdbc.StaticQuery
import com.typesafe.slick.testkit.util.{DBTest, DBTestObject, JdbcTestDB}
import com.typesafe.slick.testkit.util.StandardTestDBs._

object CodeGeneratorRoundTripTest extends DBTestObject(H2Mem, SQLiteMem, Postgres, MySQL, DerbyMem, HsqldbMem)
class CodeGeneratorRoundTripTest(val tdb: JdbcTestDB) extends DBTest {
  import tdb.profile.simple._
  import tdb.driver.quoteIdentifier
  @Test def test { db.withSession{ implicit session =>
    object Tables extends roundtrip.Tables{
      val profile = tdb.profile
    }
    import Tables.profile.simple._
    import Tables._
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

    SelfRef.forceInsert(SelfRefRow(1,None))
    SelfRef.forceInsert(SelfRefRow(2,Some(1)))
    SelfRef.run

    // Testing table larger 22 columns
    import scala.slick.collection.heterogenous._

    val oData = 0L :: 11 :: 12 :: 13 :: 14 :: 15 :: 16 :: 21 :: 22 :: 23 :: 24 :: 25 :: 26 :: 31 :: 32 :: 33 :: 34 :: 35 :: 36 :: 41 :: 42 :: 43 :: 44 :: 45 :: 46 :: 51 :: 52 :: 53 :: 54 :: 55 :: 56 :: 61 :: 62 :: 63 :: 64 :: 65 :: 66 :: HNil
    val oData2 = LargeRow( 1L, p6i4 = 123, p1i5 = 456 )
    Large.insert(oData)
    Large.insert(oData2)
    assertEquals(Set((oData,0),(oData2,1)), Large.map(r => (r,r.id)).run.toSet)
    assertEquals(oData, StaticQuery.queryNA[LargeRow]("select * from "+quoteIdentifier("LARGE")+" where "+quoteIdentifier("id")+" = 0").first )
    assertEquals(oData, StaticQuery.queryNA("select * from "+quoteIdentifier("LARGE")+" where "+quoteIdentifier("id")+" = 0")(GetResultLargeRow).first )


    X.map(r => (r.pk,r.pk2,r.column,r.schemaNameXX,r.schemaNameX)).insert((1,1,1,1.1,"test"))

    {
      // testing name and types especially in case of collisions
      import scala.slick.lifted._
      X.map(r =>{(r.pk: Rep[Int]) == null})
      X.map(r =>{(r.pk2: Rep[Int]) == null})
      X.map(r =>{(r.`val`: Rep[Option[Int]]) == null})
      X.map(r =>{(r.column: Rep[Int]) == null})
      X.map(r =>{(r.schemaNameXX: Rep[Double]) == null})
      X.map(r =>{(r.schemaNameX: Rep[String]) == null})
      X.map(r =>{(r.index1: Rep[Option[Int]]) == null})
      X.map(r =>{(r.posts: Rep[Option[Int]]) == null})
      X.map(r =>{(r.pkX: PrimaryKey) == null})
      X.map(r =>{(r.postsFk: ForeignKeyQuery[Posts,PostsRow]) == null})
      X.map(r =>{(r.categoriesFk2: ForeignKeyQuery[Categories,CategoriesRow]) == null})
      X.map(r =>{(r.categoriesFk3: ForeignKeyQuery[Categories,CategoriesRow]) == null})
      X.map(r =>{(r.index1X: Index) == null})
      X.map(r =>{(r.index2: Index) == null})
      X.map(r =>{(r.index1X: Index) == null})
      X.map(r =>{(r.index2: Index) == null})
      X.map(r =>{(r.index3: Index) == null})
      X.map(r =>{(r.index4: Index) == null})

      TypeTest.map(r =>{(r.pk: PrimaryKey) == null})
    }
  }}
}
