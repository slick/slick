package slick.test.compile

import slick.jdbc.H2Profile.api._

/** Compile-time test: a filter built from optional criteria, where an absent criterion becomes `true.bind`.
  *
  * On Scala 2, `Option.fold` takes its result type from its first argument, so the comparisons in the second
  * argument (`===`, `inSet`) must return a type that conforms to the type of `true.bind`.
  */
object OptionalFilterCriteriaTest {
  class People(tag: Tag) extends Table[(Int, String)](tag, "people") {
    def id = column[Int]("id", O.PrimaryKey)
    def name = column[String]("name")
    def * = (id, name)
  }
  val people = TableQuery[People]

  def search(name: Option[String], ids: Option[Seq[Int]]) =
    people.filter { p =>
      name.fold(true.bind)(n => p.name === n) &&
        ids.fold(true.bind)(is => p.id.inSet(is))
    }
}
