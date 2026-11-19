package slick.test.lifted

import org.junit.Test
import org.junit.Assert._
import org.junit.runner.RunWith
import org.junit.runners.Parameterized
import scala.jdk.CollectionConverters._

/** Test case for the MySQL SQL DDL overrides */
@RunWith(classOf[Parameterized])
class MySQLDDLTest(testSchema: Option[String]) {
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
    t2CreateStatements.foreach(s => assertTrue("DDL (create) uses escaped table name: " + s, s.contains(table2QualifiedName)))
    assertTrue("Fk name must be escaped", t2CreateStatements.exists(_.contains(fkEscapedName)))

    val t2DropStatements = ts2.schema.dropStatements.toList
    t2DropStatements.foreach(s => assertTrue("DDL (drop) uses escaped table name: " + s, s.contains(table2QualifiedName)))
    assertTrue("Fk name must be escaped", t2DropStatements.exists(_.contains(fkEscapedName)))

    // MySQL rejects "DROP CONSTRAINT IF EXISTS"; createIfNotExists drops the foreign key with "DROP FOREIGN KEY"
    val t2CreateIfNotExistsStatements = ts2.schema.createIfNotExistsStatements.toList
    assertTrue(
      "DDL (createIfNotExists) must drop the foreign key with DROP FOREIGN KEY: " + t2CreateIfNotExistsStatements,
      t2CreateIfNotExistsStatements.contains(s"ALTER TABLE $table2QualifiedName DROP FOREIGN KEY $fkEscapedName")
    )
    assertFalse(
      "DDL (createIfNotExists) must not use DROP CONSTRAINT IF EXISTS: " + t2CreateIfNotExistsStatements,
      t2CreateIfNotExistsStatements.exists(_.toLowerCase.contains("drop constraint if exists"))
    )
  }

  @Test def testLongStringUsesMediumText: Unit = {
    class T1687(tag: Tag) extends Table[String](tag, testSchema, "mytable1687") {
      def bar = column[String]("bar", O.Length(16777215, varying = true))

      def * = bar
    }
    val ts = TableQuery[T1687]

    val createStatements = ts.schema.createStatements.mkString("\n")
    assertTrue("DDL (create) must contain SQL statement(s)", createStatements.nonEmpty)
    assertTrue(
      "Long String column must be generated as MEDIUMTEXT, not VARCHAR: " + createStatements,
      createStatements.contains("MEDIUMTEXT")
    )
    assertFalse(
      "Long String column must not be generated as an over-long VARCHAR: " + createStatements,
      createStatements.contains("VARCHAR(16777215)")
    )
  }

  @Test def testClobUsesLongText: Unit = {
    class T637(tag: Tag) extends Table[java.sql.Clob](tag, testSchema, "mytable637") {
      def c = column[java.sql.Clob]("c")

      def * = c
    }
    val ts = TableQuery[T637]

    val createStatements = ts.schema.createStatements.mkString("\n")
    assertTrue(
      "Clob column must be generated as LONGTEXT, MySQL has no CLOB type: " + createStatements,
      createStatements.contains("`c` LONGTEXT")
    )
  }

  @Test def testTemporalColumnsKeepFractionalSeconds: Unit = {
    class T1412(tag: Tag)
      extends Table[(java.sql.Timestamp, Option[java.sql.Timestamp], java.sql.Time, Option[java.sql.Timestamp], String)](
        tag, testSchema, "mytable1412") {
      def ts = column[java.sql.Timestamp]("ts")
      def tsOpt = column[Option[java.sql.Timestamp]]("ts_opt")
      def t = column[java.sql.Time]("t")
      def tsMillis = column[Option[java.sql.Timestamp]]("ts_millis", O.SqlType("TIMESTAMP(3)"))
      def s = column[String]("s")

      def * = (ts, tsOpt, t, tsMillis, s)
    }
    val ts = TableQuery[T1412]

    val createStatements = ts.schema.createStatements.mkString("\n")
    assertTrue("Timestamp column must keep microseconds: " + createStatements,
      createStatements.contains("`ts` TIMESTAMP(6) NOT NULL"))
    // Without NULL, MySQL makes a TIMESTAMP column NOT NULL unless explicit_defaults_for_timestamp is on
    assertTrue("Nullable Timestamp column must keep microseconds and be declared NULL: " + createStatements,
      createStatements.contains("`ts_opt` TIMESTAMP(6) NULL"))
    assertTrue("Time column must keep microseconds: " + createStatements,
      createStatements.contains("`t` TIME(6) NOT NULL"))
    assertTrue("O.SqlType must override the precision, and a nullable TIMESTAMP(n) must be declared NULL: " +
      createStatements, createStatements.contains("`ts_millis` TIMESTAMP(3) NULL"))

    // The JDBC convert escape takes no precision
    val castStatement = ts.map(_.s.asColumnOf[java.sql.Timestamp]).result.statements.mkString
    assertTrue("Cast to Timestamp must use the plain type name: " + castStatement,
      castStatement.contains("`s`,TIMESTAMP)}"))
  }

  @Test def testFractionalSecondsPrecisionZeroKeepsPlainTypes: Unit = {
    object WholeSecondsProfile extends slick.jdbc.MySQLProfile {
      override protected lazy val defaultFractionalSecondsPrecision = 0
    }

    assertEquals("TIMESTAMP", WholeSecondsProfile.columnTypes.timestampJdbcType.sqlTypeName(None))
    assertEquals("TIME", WholeSecondsProfile.columnTypes.timeJdbcType.sqlTypeName(None))
  }
}

object MySQLDDLTest {
  @Parameterized.Parameters
  // test with no schema defined and a schemaName specified
  def testSchemaParameter = List[Option[String]](None, Some("schemaName")).map(Array(_)).asJava
}