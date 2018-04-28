package com.example

import org.junit.runner.RunWith
import com.typesafe.slick.testkit.util.{ExternalJdbcTestDB, TestDB, ProfileTest, Testkit}
import scala.concurrent.ExecutionContext
import slick.jdbc.ResultSetAction
import slick.dbio.DBIO
import slick.jdbc.GetResult._

//#outline
@RunWith(classOf[Testkit])
class MyPostgresTest extends ProfileTest(MyPostgresTest.tdb)

object MyPostgresTest {
  //#tdb
  def tdb = new ExternalJdbcTestDB("mypostgres") {
  //#tdb
    val profile = MyPostgresProfile
    override def localTables(implicit ec: ExecutionContext): DBIO[Vector[String]] =
      ResultSetAction[(String,String,String, String)](_.conn.getMetaData().getTables("", "public", null, null)).map { ts =>
        ts.filter(_._4.toUpperCase == "TABLE").map(_._3).sorted
      }
    override def localSequences(implicit ec: ExecutionContext): DBIO[Vector[String]] =
      ResultSetAction[(String,String,String, String)](_.conn.getMetaData().getTables("", "public", null, null)).map { ts =>
        ts.filter(_._4.toUpperCase == "SEQUENCE").map(_._3).sorted
      }
    override def capabilities = super.capabilities - TestDB.capabilities.jdbcMetaGetFunctions
  }
}
//#outline
