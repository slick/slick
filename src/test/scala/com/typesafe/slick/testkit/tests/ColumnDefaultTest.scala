package com.typesafe.slick.testkit.tests

import org.junit.Test
import org.junit.Assert._
import scala.slick.lifted._
import scala.slick.session.Database.threadLocalSession
import scala.slick.testutil.TestDB
import com.typesafe.slick.testkit.util.TestkitTest

//object ColumnDefaultTest extends TestkitTestObject(H2Mem, SQLiteMem, Postgres, MySQL, DerbyMem, HsqldbMem, SQLServer)

class ColumnDefaultTest(val tdb: TestDB) extends TestkitTest {
  import tdb.profile.Table
  import tdb.profile.Implicit._

  case class User(id: Int, first: String, last: String)

  object A extends Table[(Int, String, Option[Boolean])]("a") {
    def id = column[Int]("id")
    def a = column[String]("a", O Default "foo")
    def b = column[Option[Boolean]]("b", O Default Some(true))
    def * = id ~ a ~ b
  }

  def test = if(cap.columnDefaults) {
    db withSession {
      A.ddl.createStatements foreach println
      A.ddl.create
      A.id insert 42
      assertEquals(List((42, "foo", Some(true))), Query(A).list)
    }
  }
}
