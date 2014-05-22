package com.typesafe.slick.testkit.tests

import org.junit.Assert._
import java.sql.{Time, Date, Timestamp}
import com.typesafe.slick.testkit.util.{JdbcTestDB, TestkitTest}
import scala.slick.lifted.ColumnBase

class JdbcScalarFunctionTest extends TestkitTest[JdbcTestDB] {
  import tdb.profile.simple._

  def test {
    def check[T](q: ColumnBase[T], exp: T) = assertEquals(exp, q.run)
    def checkLit[T : ColumnType](v: T) = check(LiteralColumn(v), v)

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

  def testSubstring  {
    case class Entity(id: Int, name: String)

    class Entities(tag: Tag) extends Table[Entity](tag, "enities") {
      def id = column[Int]("id", O.PrimaryKey)

      def name = column[String]("name")

      def * = (id, name) <>(Entity.tupled, Entity.unapply)
    }

    val entities = TableQuery[Entities]

    entities.ddl.create
    entities += Entity(1, "Some")
    val names = for (s <- entities) yield s.name.substring(1, 3)
    names.foreach(println(_))
    names.foreach(n => assertEquals("Som", n))
  }
}
