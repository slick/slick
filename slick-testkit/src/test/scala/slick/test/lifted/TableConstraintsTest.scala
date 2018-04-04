package slick.test.lifted

import org.junit.Test
import org.junit.Assert._

/** Test case for the listing of constraints in table definitions */
class TableConstraintsTest {

  @Test def testConstraintsOrdering: Unit = {
    import slick.jdbc.H2Profile.api._

    class Foo(tag: Tag) extends Table[Long](tag, "foo_table") {
        def id = column[Long]("id")
        override def * = id
    }
    val foos = TableQuery[Foo]

    class Bar(tag: Tag) extends Table[Long](tag, "bar_table") {
        def id = column[Long]("id")
        override def * = id
    }
    var bars = TableQuery[Bar]

    class T(tag: Tag) extends Table[(Int, String, Long, Long)](tag, "t_table") {
      def id = column[Int]("id")
      def idIndex = index("idx_id", id)
      
      def code = column[String]("code")
      def codeIndex = index("idx_code", code)
      
      def pk1 = primaryKey("pk1_t", (id, code))
      def pk2 = primaryKey("pk2_t", (id))

      def fooId = column[Long]("foo_id")
      def foo = foreignKey("fk_foo", fooId, foos)(_.id)
      def fooIdndex = index("idx_foo_id", fooId)
      
      def barId = column[Long]("bar_id")
      def bar = foreignKey("fk_bar", barId, bars)(_.id)
      def barIdIndex = index("idx_bar_id", barId)

      override def * = (id, code, fooId, barId)
    }
    val ts = TableQuery[T]

    val foreignKeys = ts.baseTableRow.foreignKeys.map(_.name)
    assertTrue("foreignKeys should be sorted by name", foreignKeys == Seq("fk_bar", "fk_foo"))

    val primaryKeys = ts.baseTableRow.primaryKeys.map(_.name)
    assertTrue("primaryKeys should be sorted by name", primaryKeys == Seq("pk1_t", "pk2_t"))

    val indexes = ts.baseTableRow.indexes.map(_.name)
    assertTrue("indexes should be sorted by name", indexes == Seq("idx_bar_id", "idx_code", "idx_foo_id", "idx_id"))
  }
}
