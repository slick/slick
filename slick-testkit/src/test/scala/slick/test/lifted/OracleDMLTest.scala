package slick.test.lifted

import org.junit.Assert._
import org.junit.Test

class OracleDMLTest {
  import slick.jdbc.OracleProfile.api._

  @Test def testOnlyPrimaryKeysComposite(): Unit = {
    class TableWithOnlyIdColumns(tag: Tag) extends Table[(Int, Int)](tag, "mytable") {
      def id = column[Int]("id")
      def otherId = column[Int]("otherId")

      def pk = primaryKey( "SampleCompositePk", ( id, otherId ) )
      def * = (id, otherId)
    }

    assertFalse("When there are only primary keys columns there shouldn't be an update",
      TableQuery[TableWithOnlyIdColumns].insertOrUpdate((1, 1)).statements.mkString.contains("when matched then update set"))
  }

  @Test def testCompositePrimaryKeyAndOtherColumns(): Unit = {
    class TableWithOnlyIdColumns(tag: Tag) extends Table[(Int, Int, Option[String])](tag, "mytable") {
      def id = column[Int]("id")
      def otherId = column[Int]("otherId")
      def optionalColumn = column[Option[String]]("optionalColumn")


      def pk = primaryKey( "SampleCompositePk", ( id, otherId ) )
      def * = (id, otherId, optionalColumn)
    }

    assertTrue("Should update columns besides those on the primary key ",
      TableQuery[TableWithOnlyIdColumns].insertOrUpdate((1, 1, Some(""))).statements.mkString.contains("when matched then update set t.\"optionalColumn\"=s.\"optionalColumn\""))
  }

  @Test def testOnlyPrimaryKeysSinglePrimaryKey(): Unit = {
    class TableWithOnlyIdColumns(tag: Tag) extends Table[Int](tag, "mytable") {
      def id = column[Int]("id", O.PrimaryKey)

      def * = id
    }

    assertFalse("When there are only primary keys columns there shouldn't be an update",
      TableQuery[TableWithOnlyIdColumns].insertOrUpdate(1).statements.mkString.contains("when matched then update set"))
  }

  @Test def testPrimaryKeyAndOtherColumn(): Unit = {
    class TableWithOnlyIdColumns(tag: Tag) extends Table[(Int, Option[String])](tag, "mytable") {
      def id = column[Int]("id", O.PrimaryKey)
      def optionalColumn = column[Option[String]]("optionalColumn")

      def * = (id,  optionalColumn)
    }

    assertTrue("Should update columns besides those on the primary key ",
      TableQuery[TableWithOnlyIdColumns].insertOrUpdate((1, Some(""))).statements.mkString.contains("when matched then update set t.\"optionalColumn\"=s.\"optionalColumn\""))
  }

  @Test def testUnionCastsBooleansToNumbers(): Unit = {
    class T(tag: Tag) extends Table[(Int, Boolean)](tag, "t") {
      def a = column[Int]("a")
      def b = column[Boolean]("b")
      def * = (a, b)
    }
    val ts = TableQuery[T]
    // Boolean columns are CHAR(1) and Boolean literals are numbers, so union sides cast them to the same type
    val sql = ts.map(t => (t.a, t.b)).union(ts.map(t => (t.a, LiteralColumn(true)))).result.statements.mkString.replaceAll("\\s+", " ")
    assertTrue(sql, sql.contains("""cast("b" as NUMBER(1))""") && sql.contains("cast(1 as NUMBER(1))"))
  }
}
