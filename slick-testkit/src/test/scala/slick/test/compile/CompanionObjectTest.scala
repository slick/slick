package slick.test.compile

import com.typesafe.slick.testkit.util._

class CompanionObjectTest extends AsyncTest[RelationalTestDB] {
  import tdb.profile.api._

  object Person {}
  case class Person(id: Long, name: String)

  // This test should simply be able to compile.
  def testMapTo = {
    class People(tag: Tag) extends Table[Person](tag, "people") {
      def id = column[Long]("id", O.PrimaryKey, O.AutoInc)
      def name = column[String]("name")
      def * = (id, name).mapTo[Person]
    }
  }

  // Because the macro can't locate companion objects of method local case classes,
  // it's safer to leave them broken rather than make assumptions about the existence
  // of user-defined tupled methods.
  def testMapToLocal = ShouldNotTypecheck("""
    object PersonLocal {}
    case class PersonLocal(id: Long, name: String)
    class People(tag: Tag) extends Table[PersonLocal](tag, "people") {
      def id = column[Long]("id", O.PrimaryKey, O.AutoInc)
      def name = column[String]("name")
      def * = (id, name).mapTo[PersonLocal]
    }
  """, "value tupled is not a member of object PersonLocal")
}
