package slick.test.lifted

import org.junit.Test
import org.junit.Assert._
import org.junit.runner.RunWith
import org.junit.runners.Parameterized
import scala.collection.JavaConverters._

/** Test case for the MySQL SQL DDL overrides */
@RunWith(classOf[Parameterized])
class MysqlDDLTest(testSchema: Option[String]) {
  import slick.jdbc.MySQLProfile.api._

  @Test def testTablenameEscaped: Unit = {
    class T(tag: Tag) extends Table[Int](tag, testSchema, "mytable") {
      def id = column[Int]("id", O.PrimaryKey)

      def * = id
    }
    val ts = TableQuery[T]

    val table2Name = "mytable2"
    val fkName = "t_test_fk"
    class T2(tag: Tag) extends Table[Int](tag, testSchema, table2Name) {
      def id = column[Int]("id", O.PrimaryKey)

      def testFk = column[Int]("test_fk")
      def fk = foreignKey(fkName, testFk, ts)(_.id)

      def * = id
    }
    val ts2 = TableQuery[T2]

    // if schema specified, add escaped schema name to qualify the table name
    val escapedSchemaName = testSchema.map(name => s"`$name`.").getOrElse("")
    val table2QualifiedName = s"$escapedSchemaName`$table2Name`"
    val fkEscapedName = s"`$fkName`"
    val t2CreateStatements = ts2.schema.createStatements.toList
    assertTrue("DDL (create) must contain any SQL statements", t2CreateStatements.nonEmpty)
    t2CreateStatements.foreach(s => assertTrue("DDL (create) uses escaped table name: " + s, s contains table2QualifiedName))
    assertTrue("Fk name must be escaped", t2CreateStatements.exists(_.contains(fkEscapedName)))

    val t2DropStatements = ts2.schema.dropStatements.toList
    t2DropStatements.foreach(s => assertTrue("DDL (drop) uses escaped table name: " + s, s contains table2QualifiedName))
    assertTrue("Fk name must be escaped", t2DropStatements.exists(_.contains(fkEscapedName)))
  }
}

object MysqlDDLTest {
  @Parameterized.Parameters
  // test with no schema defined and a schemaName specified
  def testSchemaParameter = List[Option[String]](None, Some("schemaName")).map(Array(_)).asJava
}