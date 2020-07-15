package slick.test.jdbc

import org.junit.Test
import slick.jdbc.JdbcProfile
import org.junit.Assert._

class SqlActionBuilderTest {

  @Test
  def testStripMargin: Unit = {
    val driver = new JdbcProfile {}
    import driver.api.actionBasedSQLInterpolation

    val id = 0
    val s1 = sql"select * from SUPPLIERS where SUP_ID = ${id}"
    val s2 = sql"""select *
                  |from SUPPLIERS
                  |where SUP_ID = ${id}""".stripMargin
    val s3 = sql"""select *
                  !from SUPPLIERS
                  !where SUP_ID = ${id}""".stripMargin('!')
    val s4 = sql"""select *
                  #from SUPPLIERS
                  #where SUP_ID = ${id}""".stripMargin('#')

    def dropNewLineChars(queryParts: Seq[Any]): Seq[Any] = queryParts.map(_.asInstanceOf[String].replace('\n', ' ').replaceAll("\r", ""))

    assertEquals(s1.queryParts, dropNewLineChars(s2.queryParts))
    assertEquals(s1.queryParts, dropNewLineChars(s3.queryParts))
    assertEquals(s1.queryParts, dropNewLineChars(s4.queryParts))
  }
}
