package slick.test.compile

import com.typesafe.slick.testkit.util.ShouldNotTypecheck
import slick.jdbc.H2Profile.api._
import slick.collection.heterogeneous._

/**
 * Test case for issue #3234: Compilation hang with HNil shape mismatches for case classes >22 fields
 * 
 * This test ensures that when there's a mismatch between the number of fields in a case class
 * and the number of elements in an HList projection, the compiler produces a clear error message
 * rather than hanging indefinitely during implicit resolution.
 */
class HListShapeMismatchTest {

  def testHListShapeMismatchCompilationHang = {
    // Case class with 25 fields to trigger HList path (>22 fields)
    case class LargeCaseClass(
      f1: Int, f2: Int, f3: Int, f4: Int, f5: Int, f6: Int, f7: Int, f8: Int,
      f9: Int, f10: Int, f11: Int, f12: Int, f13: Int, f14: Int, f15: Int, f16: Int,
      f17: Int, f18: Int, f19: Int, f20: Int, f21: Int, f22: Int, f23: Int, f24: Int,
      f25: Int // Extra field that causes mismatch
    )

    // This should fail compilation with a clear error message, not hang
    // The HList has 24 elements but the case class has 25 fields
    ShouldNotTypecheck("""
      class LargeTable(tag: Tag) extends Table[LargeCaseClass](tag, "large_table") {
        def f1 = column[Int]("f1")
        def f2 = column[Int]("f2")
        def f3 = column[Int]("f3")
        def f4 = column[Int]("f4")
        def f5 = column[Int]("f5")
        def f6 = column[Int]("f6")
        def f7 = column[Int]("f7")
        def f8 = column[Int]("f8")
        def f9 = column[Int]("f9")
        def f10 = column[Int]("f10")
        def f11 = column[Int]("f11")
        def f12 = column[Int]("f12")
        def f13 = column[Int]("f13")
        def f14 = column[Int]("f14")
        def f15 = column[Int]("f15")
        def f16 = column[Int]("f16")
        def f17 = column[Int]("f17")
        def f18 = column[Int]("f18")
        def f19 = column[Int]("f19")
        def f20 = column[Int]("f20")
        def f21 = column[Int]("f21")
        def f22 = column[Int]("f22")
        def f23 = column[Int]("f23")
        def f24 = column[Int]("f24")
        // f25 is missing from the projection - this creates the mismatch

        // This projection has 24 elements but the case class has 25 fields
        // In Scala 2.13.16 without the fix, this would cause compilation to hang
        def * = (
          f1 :: f2 :: f3 :: f4 :: f5 :: f6 :: f7 :: f8 ::
          f9 :: f10 :: f11 :: f12 :: f13 :: f14 :: f15 :: f16 ::
          f17 :: f18 :: f19 :: f20 :: f21 :: f22 :: f23 :: f24 :: HNil
        ).mapTo[LargeCaseClass] // This line would hang compilation in Scala 2 without the fix
      }
    """, "type mismatch.*")
  }
}