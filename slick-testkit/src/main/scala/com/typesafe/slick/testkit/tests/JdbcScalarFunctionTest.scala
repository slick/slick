package com.typesafe.slick.testkit.tests

import org.junit.Assert._
import java.sql.{Time, Date, Timestamp}
import com.typesafe.slick.testkit.util.{JdbcTestDB, TestkitTest}
import scala.slick.lifted.ColumnBase

class JdbcScalarFunctionTest extends TestkitTest[JdbcTestDB] {

  import tdb.profile.simple._

  def test {
    def check[T](q: ColumnBase[T], exp: T) = assertEquals(exp, q.run)
    def checkLit[T: ColumnType](v: T) = check(LiteralColumn(v), v)

    checkLit(Date.valueOf("2011-07-15"))
    checkLit(Time.valueOf("15:53:21"))
    checkLit(Timestamp.valueOf("2011-07-15 15:53:21"))

    val myExpr = SimpleExpression.binary[Int, Int, Int] { (l, r, qb) =>
      qb.sqlBuilder += '('
      qb.expr(l)
      qb.sqlBuilder += '+'
      qb.expr(r)
      qb.sqlBuilder += "+1)"
    }
    check(myExpr(4, 5), 10)
  }

  case class Entity(id: Int, name: String)

  class Entities(tag: Tag) extends Table[Entity](tag, "enities") {
    def id = column[Int]("id", O.PrimaryKey)

    def name = column[String]("name")

    def * = (id, name) <>(Entity.tupled, Entity.unapply)
  }

  private val nameTest = "Some"

  private def getCollection = {
    val entities = TableQuery[Entities]
    entities.ddl.create
    entities += Entity(1, nameTest)
    entities
  }

  def testSubstring1() {
    val names = for (s <- getCollection) yield s.name.substring(1, 3)
    names.foreach(n => assertEquals(nameTest.substring(1, 3), n))
  }

  def testSubstring2() {
    val names = for (s <- getCollection) yield s.name.substring(1)
    names.foreach(n => assertEquals(nameTest.substring(1), n))
  }

  def testReplace() = ifCap(rcap.replace) {
    val names = for (s <- getCollection) yield s.name.replace("So", "Ro")
    names.foreach(n => assertEquals(nameTest.replace("So", "Ro"), n))
  }

  def testReverse() = ifCap(rcap.reverse) {
    val names = for (s <- getCollection) yield s.name.reverseString
    names.foreach(n => assertEquals(nameTest.reverse, n))
  }

  def testTake() =  {
    val names = for (s <- getCollection) yield s.name.take(3)
    names.foreach(n => assertEquals(nameTest.take(3), n))
  }

  def testDrop() = {
    val names = for (s <- getCollection) yield s.name.drop(2)
    names.foreach(n => assertEquals(nameTest.drop(2), n))
  }

  def testIndexOf() = ifCap(rcap.indexOf){
    val names = for (s <- getCollection) yield (s.name.indexOf("o"), s.name.indexOf("r"))
    names.foreach(n => {
      assertEquals(nameTest.indexOf("o"), n._1); assertEquals(nameTest.indexOf("r"), n._2)
    })
  }
}
