package slick.test.lifted

import org.junit.Assert.assertTrue
import org.junit.Test

/** Regression test for issue #1323: PostgresProfile must generate
  * `DOUBLE PRECISION` (a valid PostgreSQL type) rather than a bare `DOUBLE`
  * for a `column[Double]` in its createStatements DDL.
  */
class PostgresDDLTest {

  import slick.jdbc.PostgresProfile.api._

  @Test
  def testDoublePrecisionType: Unit = {
    class Coffees1323(tag: Tag) extends Table[(String, Double)](tag, "COFFEES_1323") {
      def name = column[String]("NAME")
      def price = column[Double]("PRICE")

      def * = (name, price)
    }
    val coffees = TableQuery[Coffees1323]

    val createStatements = coffees.schema.createStatements.toList
    assertTrue("DDL (create) must contain SQL statement(s)", createStatements.nonEmpty)

    val ddl = createStatements.mkString("\n")
    assertTrue(
      "Double column must render as DOUBLE PRECISION: " + ddl,
      ddl.contains("DOUBLE PRECISION")
    )
    assertTrue(
      "Double column must not render as a bare DOUBLE: " + ddl,
      !ddl.contains("DOUBLE NOT NULL") && !ddl.contains("DOUBLE,") && !ddl.contains("DOUBLE)")
    )
  }

}
