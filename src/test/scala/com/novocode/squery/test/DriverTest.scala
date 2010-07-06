package com.novocode.squery.test

import org.junit.Test
import org.junit.Assert._
import com.novocode.squery.combinator._
import com.novocode.squery.combinator.TypeMapper._
import com.novocode.squery.combinator.extended.{ExtendedProfile, H2Driver, OracleDriver, MySQLDriver, ExtendedTable => Table}
import com.novocode.squery.session._
import com.novocode.squery.session.Database._

object DriverTest { def main(args: Array[String]) = new DriverTest().test() }

class DriverTest {
  object Users extends Table[String]("users") {
    def name = column[String]("name", O NotNull)
    def * = name
  }

  val h2Statements = List(
    "SELECT t1.name FROM users t1 WHERE (t1.name like 'quote '' and backslash \\%' {escape '^'}) LIMIT 5",
    "SELECT t1.name FROM users t1 WHERE (t1.name like (?||'%')) LIMIT 5 OFFSET 10",
    "SELECT 42,'foo'")

  val oracleStatements = List(
    "SELECT * FROM (SELECT t1.name FROM users t1 WHERE (t1.name like 'quote '' and backslash \\%' {escape '^'})) WHERE ROWNUM <= 5",
    "SELECT * FROM (SELECT t0.*, ROWNUM ROWNUM_O FROM (t1.name,ROWNUM ROWNUM_I FROM users t1 WHERE (t1.name like (?||'%'))) t0) WHERE ROWNUM_O BETWEEN (1+10) AND (10+5) ORDER BY ROWNUM_I",
    "SELECT 42,'foo' FROM DUAL")

  val mySQLStatements = List(
    "SELECT t1.name FROM users t1 WHERE (t1.name like 'quote \\' and backslash \\\\%' {escape '^'}) LIMIT 5",
    "SELECT t1.name FROM users t1 WHERE (t1.name like concat(?,'%')) LIMIT 10,5",
    "SELECT 42,'foo' FROM DUAL")

  @Test def test() {
    runWith(H2Driver, h2Statements)
    runWith(OracleDriver, oracleStatements)
    runWith(MySQLDriver, mySQLStatements)
  }

  def runWith(profile: ExtendedProfile, expected: List[String]) {
    import profile.Implicit._
    println("Using driver: "+profile.getClass.getName)
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
