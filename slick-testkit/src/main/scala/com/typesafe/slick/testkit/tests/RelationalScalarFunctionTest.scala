package com.typesafe.slick.testkit.tests

import org.junit.Assert._
import com.typesafe.slick.testkit.util.{RelationalTestDB, TestkitTest}
import scala.slick.lifted.ColumnBase

class RelationalScalarFunctionTest extends TestkitTest[RelationalTestDB] {
  import tdb.profile.simple._

  def test {
    def check[T](q: ColumnBase[T], exp: T) = assertEquals(exp, q.run)
    def checkLit[T : ColumnType](v: T) = check(LiteralColumn(v), v)

    // Literals
    checkLit(false)
    checkLit(true)
    checkLit(42: Byte)
    checkLit(-42: Byte)
    checkLit(42)
    checkLit(-42)
    checkLit(17.5)
    checkLit(-17.5)
    checkLit(17.5f)
    checkLit(-17.5f)
    checkLit(42l)
    checkLit(-42l)
    checkLit("foo")

    check("42".asColumnOf[Int], 42)
    check(LiteralColumn("foo").length, 3)
    check(LiteralColumn("foo") ++ "bar", "foobar")
    check(LiteralColumn(1) ifNull 42, 1)
    check(LiteralColumn[Option[Int]](None) ifNull 42, 42)
    check(LiteralColumn("Foo").toUpperCase, "FOO")
    check(LiteralColumn("Foo").toLowerCase, "foo")
    check(LiteralColumn("  foo  ").ltrim, "foo  ")
    check(LiteralColumn("  foo  ").rtrim, "  foo")
    check(LiteralColumn("  foo  ").trim, "foo")
    Functions.database.toLowerCase.run
    Functions.user.toLowerCase.run
    check(LiteralColumn(8) % 3, 2)
    check(LiteralColumn(-12.5).abs, 12.5)
    check(LiteralColumn(1.9).ceil, 2.0)
    check(LiteralColumn(1.5).ceil, 2.0)
    check(LiteralColumn(1.4).ceil, 2.0)
    check(LiteralColumn(-1.9).ceil, -1.0)
    check(LiteralColumn(-1.5).ceil, -1.0)
    check(LiteralColumn(-1.4).ceil, -1.0)
    check(LiteralColumn(1.5).floor, 1.0)
    check(LiteralColumn(1.4).floor, 1.0)
    check(LiteralColumn(-1.5).floor, -2.0)
    check(LiteralColumn(-10.0).sign, -1)
    assertEquals(180.0, Functions.pi.toDegrees.run, 0.00001)
    assertTrue((Functions.pi.toDegrees.toRadians - Functions.pi).abs.run <= 0.00001)
  }
}
