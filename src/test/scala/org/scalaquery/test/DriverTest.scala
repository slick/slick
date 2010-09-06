package org.scalaquery.test

import org.junit.Test
import org.junit.Assert._
import org.scalaquery.ql._
import org.scalaquery.ql.TypeMapper._
import org.scalaquery.ql.extended.{ExtendedProfile, H2Driver, OracleDriver, MySQLDriver, ExtendedTable => Table}
import org.scalaquery.session._
import org.scalaquery.session.Database._
import org.scalaquery.test.util._
import org.scalaquery.test.util.TestDB._

object DriverTest extends DBTestObject(H2Mem, MySQL)

class DriverTest(tdb: TestDB) extends DBTest(tdb) {
  import tdb.driver.Implicit._

  object Users extends Table[String]("users") {
    def name = column[String]("name")
    def * = name
  }

  val expected: List[String] = tdb.driver match {
    case H2Driver => List(
      "SELECT \"t1\".\"name\" FROM \"users\" \"t1\" WHERE (\"t1\".\"name\" like 'quote '' and backslash \\%' {escape '^'}) LIMIT 5",
      "SELECT \"t1\".\"name\" FROM \"users\" \"t1\" WHERE (\"t1\".\"name\" like (?||'%')) LIMIT 5 OFFSET 10",
      "SELECT 42,'foo'")
    case OracleDriver => List(
      "SELECT * FROM (SELECT \"t1\".\"name\" FROM \"users\" \"t1\" WHERE (\"t1\".\"name\" like 'quote '' and backslash \\%' {escape '^'})) WHERE ROWNUM <= 5",
      "SELECT * FROM (SELECT t0.*, ROWNUM ROWNUM_O FROM (\"t1\".\"name\",ROWNUM ROWNUM_I FROM \"users\" \"t1\" WHERE (\"t1\".\"name\" like (?||'%'))) t0) WHERE ROWNUM_O BETWEEN (1+10) AND (10+5) ORDER BY ROWNUM_I",
      "SELECT 42,'foo' FROM DUAL")
    case MySQLDriver => List(
      "SELECT `t1`.`name` FROM `users` `t1` WHERE (`t1`.`name` like 'quote \\' and backslash \\\\%' {escape '^'}) LIMIT 5",
      "SELECT `t1`.`name` FROM `users` `t1` WHERE (`t1`.`name` like concat(?,'%')) LIMIT 10,5",
      "SELECT 42,'foo' FROM DUAL")
    case d => throw new RuntimeException("Unknown driver "+d)
  }

  @Test def test() {
    val q1 = Users.where(_.name startsWith "quote ' and backslash \\").take(5)
    val s1 = q1.selectStatement
    println(s1)
    assertEquals(expected(0), s1)
    val q2 = Users.where(_.name like ("St".bind ++ "%")).drop(10).take(5)
    val s2 = q2.selectStatement
    println(s2)
    assertEquals(expected(1), s2)
    val q3 = Query(42 ~ "foo")
    val s3 = q3.selectStatement
    println(s3)
    assertEquals(expected(2), s3)
  }
}
