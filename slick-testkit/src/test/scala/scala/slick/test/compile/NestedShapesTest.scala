package scala.slick.test.compile

import com.typesafe.slick.testkit.util.ShouldNotTypecheck
import scala.slick.driver.JdbcDriver.simple._
import scala.slick.lifted.{Shape, ShapeLevel}

class NestedShapesTest {
  def legal = {
    // Flat and Nested alike, only Mixed specified
    implicitly[Shape[FlatShapeLevel, Int, _, _]]
    implicitly[Shape[FlatShapeLevel, (Int, String), _, _]]
    implicitly[Shape[FlatShapeLevel, (Column[Int], Int), _, _]]
    implicitly[Shape[FlatShapeLevel, (Column[Int], (Int, Column[String])), _, _]]
    implicitly[Shape[NestedShapeLevel, Int, _, _]]
    implicitly[Shape[NestedShapeLevel, (Int, String), _, _]]
    implicitly[Shape[NestedShapeLevel, (Column[Int], Int), _, _]]
    implicitly[Shape[NestedShapeLevel, (Column[Int], (Int, Column[String])), _, _]]

    // Flat and Nested alike, fully specified
    implicitly[Shape[FlatShapeLevel, Int, Int, Column[Int]]]
    implicitly[Shape[FlatShapeLevel, (Int, String), (Int, String), (Column[Int], Column[String])]]
    implicitly[Shape[FlatShapeLevel, (Column[Int], Int), (Int, Int), (Column[Int], Column[Int])]]
    implicitly[Shape[FlatShapeLevel, (Column[Int], (Int, Column[String])), (Int, (Int, String)), (Column[Int], (Column[Int], Column[String]))]]
    implicitly[Shape[NestedShapeLevel, Int, Int, Column[Int]]]
    implicitly[Shape[NestedShapeLevel, (Int, String), (Int, String), (Column[Int], Column[String])]]
    implicitly[Shape[NestedShapeLevel, (Column[Int], Int), (Int, Int), (Column[Int], Column[Int])]]
    implicitly[Shape[NestedShapeLevel, (Column[Int], (Int, Column[String])), (Int, (Int, String)), (Column[Int], (Column[Int], Column[String]))]]

    // Only Nested, only Mixed specified
    implicitly[Shape[NestedShapeLevel, Query[Column[Int], Int], _, _]] // 1
    implicitly[Shape[NestedShapeLevel, Query[(Column[Int], Column[String]), (Int, String)], _, _]] // 2
    implicitly[Shape[NestedShapeLevel, (Column[Int], Query[(Column[Int], Column[String]), (Int, String)]), _, _]] // 3
    implicitly[Shape[NestedShapeLevel, (Int, Query[(Column[Int], Column[String]), (Int, String)]), _, _]] // 4

    // Only Nested, fully specified
    implicitly[Shape[NestedShapeLevel, Query[Column[Int], Int], Seq[Int], Query[Column[Int], Int]]] // 5
    implicitly[Shape[NestedShapeLevel, Query[(Column[Int], Column[String]), (Int, String)], Seq[(Int, String)], Query[(Column[Int], Column[String]), (Int, String)]]] // 6
    implicitly[Shape[NestedShapeLevel, (Column[Int], Query[(Column[Int], Column[String]), (Int, String)]), (Int, Seq[(Int, String)]), (Column[Int], Query[(Column[Int], Column[String]), (Int, String)])]] // 7
    implicitly[Shape[NestedShapeLevel, (Int, Query[(Column[Int], Column[String]), (Int, String)]), (Int, Seq[(Int, String)]), (Column[Int], Query[(Column[Int], Column[String]), (Int, String)])]] // 8
  }

  def illegal1 = ShouldNotTypecheck("""
      implicitly[Shape[FlatShapeLevel, Query[Column[Int], Int], _, _]] // 1
    """, "No matching Shape.*")

  def illegal2 = ShouldNotTypecheck("""
      implicitly[Shape[FlatShapeLevel, Query[(Column[Int], Column[String]), (Int, String)], _, _]] // 2
    """, "No matching Shape.*")

  def illegal3 = ShouldNotTypecheck("""
      implicitly[Shape[FlatShapeLevel, (Column[Int], Query[(Column[Int], Column[String]), (Int, String)]), _, _]] // 3
    """, "No matching Shape.*")

  def illegal4 = ShouldNotTypecheck("""
      implicitly[Shape[FlatShapeLevel, (Int, Query[(Column[Int], Column[String]), (Int, String)]), _, _]] // 4
    """, "No matching Shape.*")

  def illegal5 = ShouldNotTypecheck("""
      implicitly[Shape[FlatShapeLevel, Query[Column[Int], Int], Seq[Int], Query[Column[Int], Int]]] // 5
    """, "No matching Shape.*")

  def illegal6 = ShouldNotTypecheck("""
      implicitly[Shape[FlatShapeLevel, Query[(Column[Int], Column[String]), (Int, String)], Seq[(Int, String)], Query[(Column[Int], Column[String]), (Int, String)]]] // 6
    """, "No matching Shape.*")

  def illegal7 = ShouldNotTypecheck("""
      implicitly[Shape[FlatShapeLevel, (Column[Int], Query[(Column[Int], Column[String]), (Int, String)]), (Int, Seq[(Int, String)]), (Column[Int], Query[(Column[Int], Column[String]), (Int, String)])]] // 7
    """, "No matching Shape.*")

  def illegal8 = ShouldNotTypecheck("""
      implicitly[Shape[FlatShapeLevel, (Int, Query[(Column[Int], Column[String]), (Int, String)]), (Int, Seq[(Int, String)]), (Column[Int], Query[(Column[Int], Column[String]), (Int, String)])]] // 8
    """, "No matching Shape.*")
}
