package slick.test.lifted

import org.junit.Assert.*
import org.junit.Test

import slick.jdbc.{DB2Profile, H2Profile, JdbcProfile, SQLServerProfile}

/** `lengthLong` and `countDistinctLong` must use `count_big` on databases whose `count` returns a 32-bit INT. */
class CountBigStatementTest {
  private class Statements(val profile: JdbcProfile) {
    import profile.api.*
    class T(tag: Tag) extends Table[(Int, String)](tag, "t") {
      def id = column[Int]("id")
      def name = column[String]("name")
      def * = (id, name)
    }
    val ts = TableQuery[T]
    def all: Seq[String] = Seq(
      Query(ts.length).result.statements.head,
      Query(ts.map(_.name).countDistinct).result.statements.head,
      Query(ts.lengthLong).result.statements.head,
      Query(ts.map(_.name).countDistinctLong).result.statements.head,
      // sortBy nests the grouping, where the compiler merges identical expressions
      ts.groupBy(_.name).map { case (n, g) => (n, g.length, g.lengthLong) }.sortBy(_._1).result.statements.head
    ).map(_.toLowerCase)
  }

  private def statements(profile: JdbcProfile): Seq[String] = new Statements(profile).all

  private def check(profile: JdbcProfile): Unit = {
    val Seq(length, countDistinct, lengthLong, countDistinctLong, grouped) = statements(profile): @unchecked
    assertTrue(length, length.contains("count(") && !length.contains("count_big"))
    assertTrue(countDistinct, countDistinct.contains("count(distinct") && !countDistinct.contains("count_big"))
    assertTrue(lengthLong, lengthLong.contains("count_big(") && !lengthLong.contains("count("))
    assertTrue(countDistinctLong, countDistinctLong.contains("count_big(distinct") && !countDistinctLong.contains("count("))
    assertTrue(grouped, grouped.contains("count(") && grouped.contains("count_big("))
  }

  @Test def testSQLServer(): Unit = check(SQLServerProfile)

  @Test def testDB2(): Unit = check(DB2Profile)

  @Test def testOtherProfilesUseCount(): Unit =
    statements(H2Profile).foreach(s => assertFalse(s, s.contains("count_big")))
}
