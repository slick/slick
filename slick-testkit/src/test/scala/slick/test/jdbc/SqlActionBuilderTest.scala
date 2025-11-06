package slick.test.jdbc

import org.junit.Test
import slick.jdbc.{JdbcActionComponent, JdbcProfile}

import org.junit.Assert.*

class SqlActionBuilderTest {

  @Test
  def testStripMargin: Unit = {
    val driver = new JdbcProfile with JdbcActionComponent.MultipleRowsPerStatementSupport {}
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

    def dropNewLineChars(sql: String): String = sql.replace('\n', ' ').replaceAll("\r", "")

    assertEquals(s1.sql, dropNewLineChars(s2.sql))
    assertEquals(s1.sql, dropNewLineChars(s3.sql))
    assertEquals(s1.sql, dropNewLineChars(s4.sql))
  }
}
