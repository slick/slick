package com.typesafe.slick.testkit.tests

import org.junit.Assert._
import com.typesafe.slick.testkit.util.{RelationalTestDB, TestkitTest}

class RelationalScalarFunctionTest extends TestkitTest[RelationalTestDB] {
  import tdb.profile.simple._
  import scala.math._

  def test {
    def check[T](q: Rep[T], exp: T) = assertEquals(exp, q.run)
    def checkLit[T: ColumnType](v: T) = check(LiteralColumn(v), v)
    def checkWithPrecise[T <: Double](q: Rep[T], exp: T) = assertTrue(abs(exp - q.run) < 0.000001)

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
    // FIXME: broken in DB2, which does not seem to support nested {fn ...} calls
    // check(LiteralColumn("  foo  ").trim, "foo")
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
    check(LiteralColumn(-10.0).signum, -1)

    checkWithPrecise(LiteralColumn(9.5).exp, exp(9.5))
    checkWithPrecise(LiteralColumn(0.1).log, log(0.1))
    checkWithPrecise(LiteralColumn(1.1).log10, log10(1.1))
    checkWithPrecise(LiteralColumn(1.2).sin, sin(1.2))
    checkWithPrecise(LiteralColumn(1.1).cos, cos(1.1))
    checkWithPrecise(LiteralColumn(1.1).tan, tan(1.1))
    checkWithPrecise(LiteralColumn(1.1).cot, 1.0 / tan(1.1))
    check(LiteralColumn(6) % 4, 6 % 4)
    checkWithPrecise(LiteralColumn(1.1).atan, atan(1.1))
    checkWithPrecise(LiteralColumn(0.8).asin, asin(0.8))
    checkWithPrecise(LiteralColumn(0.8).acos, acos(0.8))
    checkWithPrecise(LiteralColumn(0.8).atan, atan(0.8))
    checkWithPrecise(LiteralColumn(4.0).sqrt, sqrt(4.0))

    assertEquals(180.0, Functions.pi.toDegrees.run, 0.00001)
    assertTrue((Functions.pi.toDegrees.toRadians - Functions.pi).abs.run <= 0.00001)
  }
}
