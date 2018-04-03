package slick.test.lifted

import org.junit.Test
import org.junit.Assert._

/** Test case for the MySQL SQL DDL overrides */
class MysqlDDLTest {
  import slick.jdbc.MySQLProfile.api._

  @Test def testTablenameEscaped: Unit = {
    class T(tag: Tag) extends Table[Int](tag, "mytable") {
      def id = column[Int]("id", O.PrimaryKey)

      def * = id
    }
    val ts = TableQuery[T]

    class T2(tag: Tag) extends Table[Int](tag, "mytable2") {
      def id = column[Int]("id", O.PrimaryKey)

      def testFk = column[Int]("test_fk")
      def fk = foreignKey("t_test_fk", testFk, ts)(_.id)

      def * = id
    }
    val ts2 = TableQuery[T2]

    val s1 = ts2.schema.createStatements.toList
    assertTrue("DDL (create) must contain any SQL statements", s1.nonEmpty)
    s1.foreach(s => assertTrue("DDL (create) uses escaped table name: " + s, s contains "`mytable2`"))
    assertTrue("Fk name must be escaped", s1.exists(_.contains("`t_test_fk`")))

    val s2 = ts2.schema.dropStatements.toList
    s2.foreach(s => assertTrue("DDL (drop) uses escaped table name: " + s, s contains "`mytable2`"))
    assertTrue("Fk name must be escaped", s2.exists(_.contains("`t_test_fk`")))
  }
}
