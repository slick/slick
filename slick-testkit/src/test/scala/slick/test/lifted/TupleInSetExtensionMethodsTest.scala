package slick.test.lifted

import org.junit.Assert._
import org.junit.Test
import slick.lifted.Tuple2OfColumnExtensionMethods

/** Test cases for Tuple inSet extension methods */
class TupleInSetExtensionMethodsTest {

  @Test def testTupleInSetExtensionMethodsTest = {
    import slick.jdbc.H2Profile.api._

    class T(tag: Tag) extends Table[(Int, String, Option[String])](tag, Some("myschema"), "mytable") {
      def id = column[Int]("id")
      def myString = column[String]("myString")
      def optString = column[Option[String]]("optString")
      def * = (id, myString, optString)
    }

    val ts = TableQuery[T]

    implicit def tuple2BaseTypedType[A: scala.math.Ordering, B: scala.math.Ordering] = new slick.ast.ScalaBaseType[(A, B)]

    val s1 = ts.filter(row => (row.id, row.myString) inSet Seq(1 -> "one", 2 -> "two")).result.statements.head
    println(s1)
    assertTrue("inSet looks correct", s1 contains """where ("id", "myString") in ((1, 'one'), (2, 'two'))""")

    // TODO - doesn't work for Option without explicit conversion
    val s2 = ts.filter(row => new Tuple2OfColumnExtensionMethods(row.id, row.optString) inSet Seq(1 -> Some("one"), 2 -> Some("two"))).result.statements.head
    println(s2)
    assertTrue("inSet looks correct for options", s2 contains """where ("id", "optString") in ((1, 'one'), (2, 'two'))""")

    // TODO - not sure if `where (foo) in (null)` is valid...
    val s3 = ts.filter(row => new Tuple2OfColumnExtensionMethods(row.id, row.optString) inSet Seq(1 -> Some("one"), 2 -> None)).result.statements.head
    println(s3)
    assertTrue("inSet looks correct for an empty option", s3 contains """where ("id", "optString") in ((1, 'one'), (2, null))""")
  }
}
