package slick.test.compile

import com.typesafe.slick.testkit.util.ShouldNotTypecheck
import slick.jdbc.H2Profile.api._
import slick.lifted.Shape

class NestedShapesTest {
  def legal = {
    // Flat and Nested alike, only Mixed specified
    implicitly[Shape[FlatShapeLevel, Int, ?, ?]]
    implicitly[Shape[FlatShapeLevel, (Int, String), ?, ?]]
    implicitly[Shape[FlatShapeLevel, (Rep[Int], Int), ?, ?]]
    implicitly[Shape[FlatShapeLevel, (Rep[Int], (Int, Rep[String])), ?, ?]]
    implicitly[Shape[NestedShapeLevel, Int, ?, ?]]
    implicitly[Shape[NestedShapeLevel, (Int, String), ?, ?]]
    implicitly[Shape[NestedShapeLevel, (Rep[Int], Int), ?, ?]]
    implicitly[Shape[NestedShapeLevel, (Rep[Int], (Int, Rep[String])), ?, ?]]

    // Flat and Nested alike, fully specified
    implicitly[Shape[FlatShapeLevel, Int, Int, Rep.TypedRep[Int]]]
    implicitly[Shape[FlatShapeLevel, (Int, String), (Int, String), (Rep.TypedRep[Int], Rep.TypedRep[String])]]
    implicitly[Shape[FlatShapeLevel, (Rep[Int], Int), (Int, Int), (Rep[Int], Rep.TypedRep[Int])]]
    implicitly[Shape[FlatShapeLevel, (Rep[Int], (Int, Rep[String])), (Int, (Int, String)), (Rep[Int], (Rep.TypedRep[Int], Rep[String]))]]
    implicitly[Shape[NestedShapeLevel, Int, Int, Rep.TypedRep[Int]]]
    implicitly[Shape[NestedShapeLevel, (Int, String), (Int, String), (Rep.TypedRep[Int], Rep.TypedRep[String])]]
    implicitly[Shape[NestedShapeLevel, (Rep[Int], Int), (Int, Int), (Rep[Int], Rep.TypedRep[Int])]]
    implicitly[Shape[NestedShapeLevel, (Rep[Int], (Int, Rep[String])), (Int, (Int, String)), (Rep[Int], (Rep.TypedRep[Int], Rep[String]))]]

    // Only Nested, only Mixed specified
    implicitly[Shape[NestedShapeLevel, Query[Rep[Int], Int, Seq], ?, ?]] // 1
    implicitly[Shape[NestedShapeLevel, Query[(Rep[Int], Rep[String]), (Int, String), Seq], ?, ?]] // 2
    implicitly[Shape[NestedShapeLevel, (Rep[Int], Query[(Rep[Int], Rep[String]), (Int, String), Seq]), ?, ?]] // 3
    implicitly[Shape[NestedShapeLevel, (Int, Query[(Rep[Int], Rep[String]), (Int, String), Seq]), ?, ?]] // 4

    // Only Nested, fully specified
    implicitly[Shape[NestedShapeLevel, Query[Rep[Int], Int, Seq], Seq[Int], Query[Rep[Int], Int, Seq]]] // 5
    implicitly[Shape[NestedShapeLevel, Query[(Rep[Int], Rep[String]), (Int, String), Seq], Seq[(Int, String)], Query[(Rep[Int], Rep[String]), (Int, String), Seq]]] // 6
    implicitly[Shape[NestedShapeLevel, (Rep[Int], Query[(Rep[Int], Rep[String]), (Int, String), Seq]), (Int, Seq[(Int, String)]), (Rep[Int], Query[(Rep[Int], Rep[String]), (Int, String), Seq])]] // 7
    implicitly[Shape[NestedShapeLevel, (Int, Query[(Rep[Int], Rep[String]), (Int, String), Seq]), (Int, Seq[(Int, String)]), (Rep.TypedRep[Int], Query[(Rep[Int], Rep[String]), (Int, String), Seq])]] // 8
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
