package scala.slick.test.compile

import com.typesafe.slick.testkit.util.ShouldNotTypecheck
import scala.slick.driver.JdbcDriver.simple._
import scala.slick.lifted.{Shape, ShapeLevel}

class NestedShapesTest {
  def legal = {
    // Flat and Nested alike, only Mixed specified
    implicitly[Shape[FlatShapeLevel, Int, _, _]]
    implicitly[Shape[FlatShapeLevel, (Int, String), _, _]]
    implicitly[Shape[FlatShapeLevel, (Rep[Int], Int), _, _]]
    implicitly[Shape[FlatShapeLevel, (Rep[Int], (Int, Rep[String])), _, _]]
    implicitly[Shape[NestedShapeLevel, Int, _, _]]
    implicitly[Shape[NestedShapeLevel, (Int, String), _, _]]
    implicitly[Shape[NestedShapeLevel, (Rep[Int], Int), _, _]]
    implicitly[Shape[NestedShapeLevel, (Rep[Int], (Int, Rep[String])), _, _]]

    // Flat and Nested alike, fully specified
    implicitly[Shape[FlatShapeLevel, Int, Int, ConstColumn[Int]]]
    implicitly[Shape[FlatShapeLevel, (Int, String), (Int, String), (ConstColumn[Int], ConstColumn[String])]]
    implicitly[Shape[FlatShapeLevel, (Rep[Int], Int), (Int, Int), (Rep[Int], ConstColumn[Int])]]
    implicitly[Shape[FlatShapeLevel, (Rep[Int], (Int, Rep[String])), (Int, (Int, String)), (Rep[Int], (ConstColumn[Int], Rep[String]))]]
    implicitly[Shape[NestedShapeLevel, Int, Int, ConstColumn[Int]]]
    implicitly[Shape[NestedShapeLevel, (Int, String), (Int, String), (ConstColumn[Int], ConstColumn[String])]]
    implicitly[Shape[NestedShapeLevel, (Rep[Int], Int), (Int, Int), (Rep[Int], ConstColumn[Int])]]
    implicitly[Shape[NestedShapeLevel, (Rep[Int], (Int, Rep[String])), (Int, (Int, String)), (Rep[Int], (ConstColumn[Int], Rep[String]))]]

    // Only Nested, only Mixed specified
    implicitly[Shape[NestedShapeLevel, Query[Rep[Int], Int, Seq], _, _]] // 1
    implicitly[Shape[NestedShapeLevel, Query[(Rep[Int], Rep[String]), (Int, String), Seq], _, _]] // 2
    implicitly[Shape[NestedShapeLevel, (Rep[Int], Query[(Rep[Int], Rep[String]), (Int, String), Seq]), _, _]] // 3
    implicitly[Shape[NestedShapeLevel, (Int, Query[(Rep[Int], Rep[String]), (Int, String), Seq]), _, _]] // 4

    // Only Nested, fully specified
    implicitly[Shape[NestedShapeLevel, Query[Rep[Int], Int, Seq], Seq[Int], Query[Rep[Int], Int, Seq]]] // 5
    implicitly[Shape[NestedShapeLevel, Query[(Rep[Int], Rep[String]), (Int, String), Seq], Seq[(Int, String)], Query[(Rep[Int], Rep[String]), (Int, String), Seq]]] // 6
    implicitly[Shape[NestedShapeLevel, (Rep[Int], Query[(Rep[Int], Rep[String]), (Int, String), Seq]), (Int, Seq[(Int, String)]), (Rep[Int], Query[(Rep[Int], Rep[String]), (Int, String), Seq])]] // 7
    implicitly[Shape[NestedShapeLevel, (Int, Query[(Rep[Int], Rep[String]), (Int, String), Seq]), (Int, Seq[(Int, String)]), (ConstColumn[Int], Query[(Rep[Int], Rep[String]), (Int, String), Seq])]] // 8
  }

  def illegal1 = ShouldNotTypecheck("""
      implicitly[Shape[FlatShapeLevel, Query[Rep[Int], Int, Seq], _, _]] // 1
    """, "No matching Shape.*")

  def illegal2 = ShouldNotTypecheck("""
      implicitly[Shape[FlatShapeLevel, Query[(Rep[Int], Rep[String]), (Int, String), Seq], _, _]] // 2
    """, "No matching Shape.*")

  def illegal3 = ShouldNotTypecheck("""
      implicitly[Shape[FlatShapeLevel, (Rep[Int], Query[(Rep[Int], Rep[String]), (Int, String), Seq]), _, _]] // 3
    """, "No matching Shape.*")

  def illegal4 = ShouldNotTypecheck("""
      implicitly[Shape[FlatShapeLevel, (Int, Query[(Rep[Int], Rep[String]), (Int, String), Seq]), _, _]] // 4
    """, "No matching Shape.*")

  def illegal5 = ShouldNotTypecheck("""
      implicitly[Shape[FlatShapeLevel, Query[Rep[Int], Int, Seq], Seq[Int], Query[Rep[Int], Int, Seq]]] // 5
    """, "No matching Shape.*")

  def illegal6 = ShouldNotTypecheck("""
      implicitly[Shape[FlatShapeLevel, Query[(Rep[Int], Rep[String]), (Int, String), Seq], Seq[(Int, String)], Query[(Rep[Int], Rep[String]), (Int, String), Seq]]] // 6
    """, "No matching Shape.*")

  def illegal7 = ShouldNotTypecheck("""
      implicitly[Shape[FlatShapeLevel, (Rep[Int], Query[(Rep[Int], Rep[String]), (Int, String), Seq]), (Int, Seq[(Int, String)]), (Rep[Int], Query[(Rep[Int], Rep[String]), (Int, String), Seq])]] // 7
    """, "No matching Shape.*")

  def illegal8 = ShouldNotTypecheck("""
      implicitly[Shape[FlatShapeLevel, (Int, Query[(Rep[Int], Rep[String]), (Int, String), Seq]), (Int, Seq[(Int, String)]), (ConstColumn[Int], Query[(Rep[Int], Rep[String]), (Int, String), Seq])]] // 8
    """, "No matching Shape.*")
}
