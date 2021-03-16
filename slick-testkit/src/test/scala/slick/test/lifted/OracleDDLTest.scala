package slick.test.lifted

import org.junit.Assert.assertTrue
import org.junit.Test

class OracleDDLTest {

  import slick.jdbc.OracleProfile.api._

  @Test
  def testDefaultEscaped: Unit = {
    class T(tag: Tag) extends Table[String](tag, Some("myschema"), "mytable") {
      def id = column[String]("id", O.PrimaryKey, O.Default("N"))

      def * = id
    }
    val ts = TableQuery[T]
    val tCreateIfNotExistsStatements = ts.schema.createIfNotExistsStatements.toList
    assertTrue("DDL (createIfNotExists) must contain SQL statement(s)", tCreateIfNotExistsStatements.nonEmpty)
    assertTrue("Default must be escaped", tCreateIfNotExistsStatements.exists(_.contains("''N''")))
  }

}
