package slick.test.compile

import com.typesafe.slick.testkit.util.ShouldNotTypecheck
import org.junit.Test
import slick.jdbc.H2Profile.api._

/** `filter` and its variants accept only `Rep[Boolean]` and `Rep[Option[Boolean]]` predicates, while
  * `withFilter` (used by for-comprehensions) still accepts a plain `Boolean`.
  *
  * The illegal cases are JUnit tests because `ShouldNotTypecheck` checks at compile time on Scala 2 but
  * asserts at run time on Scala 3. */
class FilterConditionTest {
  class T(tag: Tag) extends Table[(Int, String, Option[Boolean])](tag, "t") {
    def id = column[Int]("id")
    def name = column[String]("name")
    def flag = column[Option[Boolean]]("flag")
    def * = (id, name, flag)
  }
  val ts = TableQuery[T]

  def legal = {
    ts.filter(_.name === "x")
    ts.filter(_.flag)
    ts.filterNot(_.flag)
    ts.filterOpt(Some("x"))(_.name === _)
    ts.filterIf(true)(_.flag)
    ts.filterNotOpt(Some("x"))(_.name === _)
    ts.filterNotIf(true)(_.name === "x")
    ts.filter(_ => LiteralColumn(true))
    ts.map(_.id.?).map(_.filter(_ > 1))
    ts.withFilter(_ => true)
    ts.join(ts).on(_.flag && _.flag)
    ts.map(t => Case.If(t.id === 1).Then(1).If(t.flag).Then(2))

    for {
      (id, name) <- ts.map(t => (t.id, t.name))
      if name === "x"
    } yield id
  }

  @Test def illegalFilter(): Unit = ShouldNotTypecheck("""
      ts.filter(_.name == "x")
    """, ".*Boolean.*")

  @Test def illegalFilterConstant(): Unit = ShouldNotTypecheck("""
      ts.filter(_ => true)
    """, ".*Boolean.*")

  @Test def illegalFilterNot(): Unit = ShouldNotTypecheck("""
      ts.filterNot(_.name == "x")
    """, ".*Boolean.*")

  @Test def illegalFilterOpt(): Unit = ShouldNotTypecheck("""
      ts.filterOpt(Some("x"))(_.name == _)
    """, ".*Boolean.*")

  @Test def illegalFilterIf(): Unit = ShouldNotTypecheck("""
      ts.filterIf(true)(_.name == "x")
    """, ".*Boolean.*")

  @Test def illegalFilterNotOpt(): Unit = ShouldNotTypecheck("""
      ts.filterNotOpt(Some("x"))(_.name == _)
    """, ".*Boolean.*")

  @Test def illegalFilterNotIf(): Unit = ShouldNotTypecheck("""
      ts.filterNotIf(true)(_.name == "x")
    """, ".*Boolean.*")

  @Test def illegalOptionFilter(): Unit = ShouldNotTypecheck("""
      ts.map(_.id.?).map(_.filter(_ == 1))
    """, ".*Boolean.*")

  @Test def illegalJoinCondition(): Unit = ShouldNotTypecheck("""
      ts.join(ts).on(_.id == _.id)
    """, ".*Boolean.*")

  @Test def illegalCaseIf(): Unit = ShouldNotTypecheck("""
      ts.map(t => Case.If(t.id == 1).Then(1))
    """, ".*Boolean.*")

  @Test def illegalTypedCaseIf(): Unit = ShouldNotTypecheck("""
      ts.map(t => Case.If(t.id === 1).Then(1).If(t.id == 2).Then(2))
    """, ".*Boolean.*")
}
