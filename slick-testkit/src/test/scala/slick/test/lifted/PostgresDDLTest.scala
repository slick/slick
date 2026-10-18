package slick.test.lifted

import org.junit.Test
import org.junit.Assert._

/** Test case for the PostgreSQL SQL DDL overrides */
class PostgresDDLTest {
  import slick.jdbc.PostgresProfile.api._

  @Test def testClobUsesLargeObject: Unit = {
    class T637(tag: Tag) extends Table[java.sql.Clob](tag, "mytable637") {
      def c = column[java.sql.Clob]("c")

      def * = c
    }
    val ts = TableQuery[T637]

    // PostgreSQL has no CLOB type. The JDBC driver reads and writes Clob values as large objects
    // (by OID), so a Clob column uses the "lo" type and lo_manage trigger, the same as Blob.
    val createStatements = ts.schema.createStatements.toList
    assertTrue(
      "Clob column must be generated as lo, not CLOB: " + createStatements,
      createStatements.exists(_.contains("\"c\" lo"))
    )
    assertTrue(
      "Clob column must get a lo_manage trigger: " + createStatements,
      createStatements.exists(_.contains("lo_manage(\"c\")"))
    )
  }
}
