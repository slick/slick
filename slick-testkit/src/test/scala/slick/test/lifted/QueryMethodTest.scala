package slick.test.lifted

import org.junit.Assert._
import org.junit.Test

/** Test cases for filterIf and filterOpt */
class QueryMethodTest {

  @Test def testQueryMethod = {
    import slick.jdbc.H2Profile.api._

    class T(tag: Tag) extends Table[(Int, String, Option[String], Double)](tag, Some("myschema"), "mytable") {
      def id = column[Int]("id")
      def myString = column[String]("myString")
      def optString = column[Option[String]]("optString")
      def price = column[Double]("PRICE")
      def * = (id, myString, optString, price)
    }

    val ts = TableQuery[T]

    val s1 = ts.filterOpt(Option.empty[String])(_.myString === _).result.statements.head
    println(s1)
    assertTrue("filterOpt adds no condition when the given option is empty", s1 endsWith """from "myschema"."mytable"""")

    val s2 = ts.filterOpt(Some("something"))(_.myString === _).result.statements.head
    println(s2)
    assertTrue("filterOpt adds a condition when the given option is present", s2 endsWith """where "myString" = 'something'""")
    
    val s3 = ts.filterOpt(Some("something"))(_.myString === _).filterOpt(Option.empty[String])(_.myString like _).filterOpt(Some("something"))(_.myString startsWith _).result.statements.head
    println(s3)
    assertTrue("filterOpt stacks", s3 endsWith """where ("myString" = 'something') and ("myString" like 'something%' escape '^')""")

    val s4 = ts.filterIf(false)(_.myString == "wonderful").result.statements.head
    println(s4)
    assertTrue("filterIf adds no condition when the given option is present", s4 endsWith """from "myschema"."mytable"""")

    val s5 = ts.filterIf(true)(_.myString startsWith "foo").result.statements.head
    println(s5)
    assertTrue("filterIf adds a condition when the given option is present", s5 endsWith """where "myString" like 'foo%' escape '^'""")

    val s6 = ts.filterIf(false)(_.myString endsWith "nope").filterIf(true)(_.myString === "stack").filterIf(true)(_.myString endsWith "yes").result.statements.head
    println(s6)
    assertTrue("filterIf stacks", s6 endsWith """where ("myString" = 'stack') and ("myString" like '%yes' escape '^')""")

    val s7 = ts.filterIf(true){ d => (d.id,d.price) inSet Seq(1->1.0, 2->2.0)}.filterIf(true)(_.myString === "stack").result.statements.head
    println(s7)
    assertTrue("filter with inSet on multiple columns", s7 endsWith """where (("id", "PRICE") in ((1, 1.0), (2, 2.0))) and ("myString" = 'stack')""")

    val s8 = ts.filterIf(true){ d => (d.id,d.price) inSetBind Seq(1->1.0, 2->2.0)}.filterIf(true)(_.myString === "stack").result.statements.head
    println(s8)
    assertTrue("filter with inSet on multiple columns", s8 endsWith """where (("id", "PRICE") in ((?, ?), (?, ?))) and ("myString" = 'stack')""")

  }
}