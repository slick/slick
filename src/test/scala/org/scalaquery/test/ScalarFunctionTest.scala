package org.scalaquery.test

import org.junit.Test
import org.junit.Assert._
import org.junit.runner.JUnitCore
import org.scalaquery.ql._
import org.scalaquery.ql.TypeMapper._
import org.scalaquery.ql.basic.{BasicTable => Table}
import org.scalaquery.ql.extended.DerbyDriver
import org.scalaquery.session._
import org.scalaquery.session.Database.threadLocalSession
import org.scalaquery.test.util._
import org.scalaquery.test.util.TestDB._

object ScalarFunctionTest extends DBTestObject(H2Mem, Postgres, MySQL, DerbyMem, HsqldbMem)

class ScalarFunctionTest(tdb: TestDB) extends DBTest(tdb) {
  import tdb.driver.Implicit._

  @Test def test = db withSession {
    def check[T](q: Query[ColumnBase[T]], exp: T*) = {
      println("Executing: " + q.selectStatement)
      assertEquals(exp.toSet, q.list.toSet)
    }

    check(Query("42".asColumnOf[Int]), 42)
    check(Query(ConstColumn("foo").length), 3)
    check(Query(ConstColumn("foo") ++ "bar"), "foobar")
    check(Query(ConstColumn(1) ifNull 42), 1)
    check(Query(ConstColumn[Option[Int]](None) ifNull 42), 42)
    check(Query(ConstColumn("Foo").toUpperCase), "FOO")
    check(Query(ConstColumn("Foo").toLowerCase), "foo")
    check(Query(ConstColumn("  foo  ").ltrim), "foo  ")
    check(Query(ConstColumn("  foo  ").rtrim), "  foo")
    check(Query(ConstColumn("  foo  ").trim), "foo")
    if(tdb.driver != DerbyDriver) {
      // Derby does not support {fn database()}
      check(Query(Functions.database.toLowerCase), tdb.dbName.toLowerCase)
    }
    check(Query(Functions.user.toLowerCase), tdb.userName.toLowerCase)
    check(Query(ConstColumn(8) % 3 ), 2)
    check(Query(ConstColumn(-12.5).abs), 12.5)
    check(Query(ConstColumn(1.5).ceil), 2.0)
    check(Query(ConstColumn(1.5).floor), 1.0)
    check(Query(ConstColumn(-10.0).sign), -1)
    check(Query(Functions.pi.toDegrees), 180.0)
    check(Query(Functions.pi.toDegrees.toRadians is Functions.pi), true)
  }
}
