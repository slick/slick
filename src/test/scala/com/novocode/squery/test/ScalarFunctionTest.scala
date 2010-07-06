package com.novocode.squery.test

import org.junit.Test
import org.junit.Assert._
import org.junit.runner.JUnitCore
import com.novocode.squery.combinator._
import com.novocode.squery.combinator.TypeMapper._
import com.novocode.squery.combinator.basic.BasicDriver.Implicit._
import com.novocode.squery.combinator.basic.{BasicTable => Table}
import com.novocode.squery.session._
import com.novocode.squery.session.Database.threadLocalSession

object ScalarFunctionTest {
  def main(args: Array[String]) = JUnitCore.main(Array(classOf[ScalarFunctionTest].getName):_*);
}

class ScalarFunctionTest {
  @Test def testConvert = check(Query("42".asColumnOf[Int]), 42)
  @Test def testLength = check(Query(ConstColumn("foo").length), 3)
  @Test def testConcat = check(Query(ConstColumn("foo") ++ "bar"), "foobar")
  @Test def testIfNull1 = check(Query(ConstColumn(1) ifNull 42), 1)
  @Test def testIfNull2 = check(Query(ConstColumn[Option[Int]](None) ifNull 42), 42)
  @Test def testToUpperCase = check(Query(ConstColumn("Foo").toUpperCase), "FOO")
  @Test def testToLowerCase = check(Query(ConstColumn("Foo").toLowerCase), "foo")
  @Test def testLTrim = check(Query(ConstColumn("  foo  ").ltrim), "foo  ")
  @Test def testRTrim = check(Query(ConstColumn("  foo  ").rtrim), "  foo")
  @Test def testTrim = check(Query(ConstColumn("  foo  ").trim), "foo")
  @Test def testDatabase = check(Query(Functions.database.toLowerCase), "test1")
  @Test def testUser = check(Query(Functions.user), "")
  @Test def testMod = check(Query(ConstColumn(8) % 3 ), 2)
  @Test def testAbs = check(Query(ConstColumn(-12.5).abs), 12.5)
  @Test def testCeil = check(Query(ConstColumn(1.5).ceil), 2.0)
  @Test def testFloor = check(Query(ConstColumn(1.5).floor), 1.0)
  @Test def testSign = check(Query(ConstColumn(-10.0).sign), -1)
  @Test def testToDegrees = check(Query(Functions.pi.toDegrees), 180.0)
  @Test def testToRadians = check(Query(Functions.pi.toDegrees.toRadians is Functions.pi), true)

  def run(f: => Unit) = Database.forURL("jdbc:h2:mem:test1", driver = "org.h2.Driver").withSession(f)
  def check[T](q: Query[ColumnBase[T]], exp: T*) = run(assertEquals(exp.toSet, q.list.toSet))
}
