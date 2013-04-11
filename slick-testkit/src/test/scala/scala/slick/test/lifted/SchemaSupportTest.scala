package scala.slick.test.lifted

import org.junit.Test
import org.junit.Assert._

/** Test case for the SQL schema support in table definitions */
class SchemaSupportTest {

  @Test def testSchemaSupport {
    import scala.slick.driver.H2Driver.simple._

    object T extends Table[Int](Some("myschema"), "mytable") {
      def id = column[Int]("id")
      def * = id
    }

    val s1 = Query(T).filter(_.id < 5).selectStatement
    println(s1)
    assertTrue("select ... from uses schema name", s1 contains """from "myschema"."mytable" """)

    //val s2 = T.insertStatement
    //println(s2)

    val s3 = Query(T).filter(_.id < 5).updateStatement
    println(s3)
    assertTrue("update uses schema name", s3 contains """update "myschema"."mytable" """)

    val s4 = Query(T).filter(_.id < 5).deleteStatement
    println(s4)
    assertTrue("delete uses schema name", s4 contains """delete from "myschema"."mytable" """)

    val s5 = T.ddl.createStatements
    s5.foreach(println)
    s5.foreach(s => assertTrue("DDL (create) uses schema name", s contains """ "myschema"."mytable" """))

    val s6 = T.ddl.dropStatements
    s6.foreach(println)
    s6.foreach(s => assertTrue("DDL (drop) uses schema name", s contains """ "myschema"."mytable" """))
  }
}
