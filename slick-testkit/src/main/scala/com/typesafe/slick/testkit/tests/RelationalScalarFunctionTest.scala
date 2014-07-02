package com.typesafe.slick.testkit.tests

import org.junit.Assert._
import com.typesafe.slick.testkit.util.{RelationalTestDB, TestkitTest}

class RelationalScalarFunctionTest extends TestkitTest[RelationalTestDB] {
  import tdb.profile.simple._

  def test {
    def check[T](q: Rep[T], exp: T) = assertEquals(exp, q.run)
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
    check(LiteralColumn(-10.0).sign, -1)
    assertEquals(180.0, Functions.pi.toDegrees.run, 0.00001)
    assertTrue((Functions.pi.toDegrees.toRadians - Functions.pi).abs.run <= 0.00001)
  }

  case class Entity(id: Int, name: String)

  class Entities(tag: Tag) extends Table[Entity](tag, "enities") {
    def id = column[Int]("id", O.PrimaryKey)

    def name = column[String]("name")

    def * = (id, name) <>(Entity.tupled, Entity.unapply)
  }

  private val nameTest = "abcdefghijklmnopqrstuvwxyz"

  private def getCollection = {
    val entities = TableQuery[Entities]
    entities.ddl.create
    entities += Entity(1, nameTest)
    entities
  }

  def testSubstring1() {
    val names = for (s <- getCollection) yield s.name.substring(3, 5)
    names.run.foreach(n => assertEquals(nameTest.substring(3, 5), n))
  }

  def testSubstring2() {
    val names = for (s <- getCollection) yield s.name.substring(3)
    names.run.foreach(n => assertEquals(nameTest.substring(3), n))
  }

  def testReplace() = ifCap(rcap.replace) {
    val names = for (s <- getCollection) yield s.name.replace("cd", "XXXX")
    names.run.foreach(n => assertEquals(nameTest.replace("cd", "XXXX"), n))
  }

  def testReverse() = ifCap(rcap.reverse) {
    val names = for (s <- getCollection) yield s.name.reverseString
    names.run.foreach(n => assertEquals(nameTest.reverse, n))
  }

  def testTake() =  {
    val names = for (s <- getCollection) yield s.name.take(3)
    names.run.foreach(n => assertEquals(nameTest.take(3), n))
  }

  def testDrop() = {
    val names = for (s <- getCollection) yield s.name.drop(3)
    names.run.foreach(n => assertEquals(nameTest.drop(3), n))
  }

  def testIndexOf() = ifCap(rcap.indexOf){
    val names = for (s <- getCollection) yield (s.name.indexOf("o"), s.name.indexOf("r"))
    names.run.foreach(n => {
      assertEquals(nameTest.indexOf("o"), n._1); assertEquals(nameTest.indexOf("r"), n._2)
    })
  }
}
