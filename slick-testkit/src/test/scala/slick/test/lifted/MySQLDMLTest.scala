package slick.test.lifted

import org.junit.Test
import org.junit.Assert._

class MySQLDMLTest {
  import slick.jdbc.MySQLProfile.api._

  @Test def testInsertOrUpdate(): Unit = {
    class T(tag: Tag) extends Table[Int](tag, "mytable") {
      def id = column[Int]("id", O.PrimaryKey, O.AutoInc)
      def * = id
    }
    case class V(id: String, value: String)
    class T2(tag: Tag) extends Table[V](tag, "mytable2") {
      def id = column[String]("id", O.PrimaryKey)
      def value = column[String]("value")
      def * = (id, value) <> ((V.apply _).tupled, V.unapply)
    }
    assertTrue("generates query with update action if insert primary key only",
      TableQuery[T].insertOrUpdate(1).statements.mkString.endsWith("on duplicate key update `id`=VALUES(`id`)"))
    assertTrue("generates query with update action if insert with value",
      TableQuery[T2].insertOrUpdate(V("test", "test-value")).statements.mkString.endsWith("on duplicate key update `value`=VALUES(`value`)"))
  }
}
