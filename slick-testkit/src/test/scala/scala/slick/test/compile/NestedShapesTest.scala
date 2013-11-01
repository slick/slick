package scala.slick.test.compile

import com.typesafe.slick.testkit.util.ShouldNotTypecheck
import scala.slick.driver.JdbcDriver.simple._
import scala.slick.lifted.{Shape, ShapeLevel}

class NestedShapesTest {
  def legal = {
    // Flat and Nested alike, only Mixed specified
    implicitly[Shape[ShapeLevel.Flat, Int, _, _]]
    implicitly[Shape[ShapeLevel.Flat, (Int, String), _, _]]
    implicitly[Shape[ShapeLevel.Flat, (Column[Int], Int), _, _]]
    implicitly[Shape[ShapeLevel.Flat, (Column[Int], (Int, Column[String])), _, _]]
    implicitly[Shape[ShapeLevel.Nested, Int, _, _]]
    implicitly[Shape[ShapeLevel.Nested, (Int, String), _, _]]
    implicitly[Shape[ShapeLevel.Nested, (Column[Int], Int), _, _]]
    implicitly[Shape[ShapeLevel.Nested, (Column[Int], (Int, Column[String])), _, _]]

    // Flat and Nested alike, fully specified
    implicitly[Shape[ShapeLevel.Flat, Int, Int, Column[Int]]]
    implicitly[Shape[ShapeLevel.Flat, (Int, String), (Int, String), (Column[Int], Column[String])]]
    implicitly[Shape[ShapeLevel.Flat, (Column[Int], Int), (Int, Int), (Column[Int], Column[Int])]]
    implicitly[Shape[ShapeLevel.Flat, (Column[Int], (Int, Column[String])), (Int, (Int, String)), (Column[Int], (Column[Int], Column[String]))]]
    implicitly[Shape[ShapeLevel.Nested, Int, Int, Column[Int]]]
    implicitly[Shape[ShapeLevel.Nested, (Int, String), (Int, String), (Column[Int], Column[String])]]
    implicitly[Shape[ShapeLevel.Nested, (Column[Int], Int), (Int, Int), (Column[Int], Column[Int])]]
    implicitly[Shape[ShapeLevel.Nested, (Column[Int], (Int, Column[String])), (Int, (Int, String)), (Column[Int], (Column[Int], Column[String]))]]

    // Only Nested, only Mixed specified
    implicitly[Shape[ShapeLevel.Nested, Query[Column[Int], Int], _, _]] // 1
    implicitly[Shape[ShapeLevel.Nested, Query[(Column[Int], Column[String]), (Int, String)], _, _]] // 2
    implicitly[Shape[ShapeLevel.Nested, (Column[Int], Query[(Column[Int], Column[String]), (Int, String)]), _, _]] // 3
    implicitly[Shape[ShapeLevel.Nested, (Int, Query[(Column[Int], Column[String]), (Int, String)]), _, _]] // 4

    // Only Nested, fully specified
    implicitly[Shape[ShapeLevel.Nested, Query[Column[Int], Int], Seq[Int], Query[Column[Int], Int]]] // 5
    implicitly[Shape[ShapeLevel.Nested, Query[(Column[Int], Column[String]), (Int, String)], Seq[(Int, String)], Query[(Column[Int], Column[String]), (Int, String)]]] // 6
    implicitly[Shape[ShapeLevel.Nested, (Column[Int], Query[(Column[Int], Column[String]), (Int, String)]), (Int, Seq[(Int, String)]), (Column[Int], Query[(Column[Int], Column[String]), (Int, String)])]] // 7
    implicitly[Shape[ShapeLevel.Nested, (Int, Query[(Column[Int], Column[String]), (Int, String)]), (Int, Seq[(Int, String)]), (Column[Int], Query[(Column[Int], Column[String]), (Int, String)])]] // 8
  }

  def illegal1 = ShouldNotTypecheck("""
      implicitly[Shape[ShapeLevel.Flat, Query[Column[Int], Int], _, _]] // 1
    """, "No matching Shape.*")

  def illegal2 = ShouldNotTypecheck("""
      implicitly[Shape[ShapeLevel.Flat, Query[(Column[Int], Column[String]), (Int, String)], _, _]] // 2
    """, "No matching Shape.*")

  def illegal3 = ShouldNotTypecheck("""
      implicitly[Shape[ShapeLevel.Flat, (Column[Int], Query[(Column[Int], Column[String]), (Int, String)]), _, _]] // 3
    """, "No matching Shape.*")

  def illegal4 = ShouldNotTypecheck("""
      implicitly[Shape[ShapeLevel.Flat, (Int, Query[(Column[Int], Column[String]), (Int, String)]), _, _]] // 4
    """, "No matching Shape.*")

  def illegal5 = ShouldNotTypecheck("""
      implicitly[Shape[ShapeLevel.Flat, Query[Column[Int], Int], Seq[Int], Query[Column[Int], Int]]] // 5
    """, "No matching Shape.*")

  def illegal6 = ShouldNotTypecheck("""
      implicitly[Shape[ShapeLevel.Flat, Query[(Column[Int], Column[String]), (Int, String)], Seq[(Int, String)], Query[(Column[Int], Column[String]), (Int, String)]]] // 6
    """, "No matching Shape.*")

  def illegal7 = ShouldNotTypecheck("""
      implicitly[Shape[ShapeLevel.Flat, (Column[Int], Query[(Column[Int], Column[String]), (Int, String)]), (Int, Seq[(Int, String)]), (Column[Int], Query[(Column[Int], Column[String]), (Int, String)])]] // 7
    """, "No matching Shape.*")

  def illegal8 = ShouldNotTypecheck("""
      implicitly[Shape[ShapeLevel.Flat, (Int, Query[(Column[Int], Column[String]), (Int, String)]), (Int, Seq[(Int, String)]), (Column[Int], Query[(Column[Int], Column[String]), (Int, String)])]] // 8
    """, "No matching Shape.*")
}
