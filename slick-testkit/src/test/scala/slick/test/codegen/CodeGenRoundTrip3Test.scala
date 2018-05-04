package slick.test.codegen

import com.typesafe.slick.testkit.util.StandardTestDBs._
import com.typesafe.slick.testkit.util.{DBTest, DBTestObject, JdbcTestDB}
import org.junit.Assert._
import org.junit.Test

import scala.concurrent.ExecutionContext.Implicits.global

object CodeGeneratorRoundTrip3Test extends DBTestObject(H2Mem, SQLiteMem, Postgres, MySQL, DerbyMem, HsqldbMem, SQLServerJTDS, SQLServerSQLJDBC)

class CodeGeneratorRoundTrip3Test(val tdb: JdbcTestDB) extends DBTest {
  import tdb.profile.quoteIdentifier

  @Test def test: Unit = runBlocking {
    object Tables extends roundtrip.Tables { val profile = tdb.profile }
    import Tables._
    import Tables.profile.api._
    GetResultPostsRow
    GetResultLargeRow

    DBIO.seq(
      schema.create,
      Categories += CategoriesRow(1,"cat"),
      Posts ++= Seq(
        PostsRow(1,"post 1",Some(1)),
        PostsRow(2,"post 2",Some(1)),
        PostsRow(3,"post 3",Some(1))
      ),
      Categories += CategoriesRow(2,"cat"),
      Posts.length.result.zip(Posts.filter(_.title =!= "post 1").map(_.title).to[List].result).map(res => assertEquals((3,List("post 2","post 3")), res)),
      sql"""select * from #${quoteIdentifier("POSTS")} where #${quoteIdentifier("id")} = 2""".as[PostsRow].head.map(res => assertEquals(PostsRow(2,"post 2",Some(1)), res)),
      {
        import slick.jdbc.JdbcCapabilities
        if(tdb.profile.capabilities.contains(JdbcCapabilities.forceInsert)){
          DBIO.seq(
            SelfRef.forceInsert(SelfRefRow(1,None)),
            SelfRef.forceInsert(SelfRefRow(2,Some(1))))
        }else
          DBIO.seq()
      },
      SelfRef.result,
      {
        // Testing table larger 22 columns
        val oData = LargeRow( 0L, 11, 12, 13, 14, 15, 16, 21, 22, 23, 24, 25, 26, 31, 32, 33, 34, 35, 36, 41, 42, 43, 44, 45, 46, 51, 52, 53, 54, 55, 56, 61, 62, 63, 64, 65, 66 )
        val oData2 = LargeRow( 1L, p6i4 = 123, p1i5 = 456 )
        DBIO.seq(
          Large += oData,
          Large += oData2,
          sql"""select * from #${quoteIdentifier("LARGE")} where #${quoteIdentifier("id")} = 0""".as[LargeRow].head.map(res => assertEquals(oData, res))
        )
      },
      (X.map(r => (r.pk,r.pk2,r.column,r.schemaNameXX,r.schemaNameX)) += (1,1,1,1.1,"test")).map { _ =>
        // testing name and types especially in case of collisions
        import slick.lifted._
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
    ).withPinnedSession
  }
}
