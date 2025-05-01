package slick.test.compile

import com.typesafe.slick.testkit.util._

class CompanionObjectTest extends AsyncTest[RelationalTestDB] {
  import tdb.profile.api._

  object Person {}
  case class Person(id: Long, name: String)

  // This test should simply be able to compile. The mapTo macro should be able to resolve
  // code that compiles even with a hand-written companion object.
  def testMapTo = {
    class People(tag: Tag) extends Table[Person](tag, "people") {
      def id = column[Long]("id", O.PrimaryKey, O.AutoInc)
      def name = column[String]("name")
      def * = (id, name).<>((Person.apply _).tupled, Person.unapply)
    }
  }

  // This test should simply be able to compile. The mapTo macro should work for method-local
  // case classes with hand-written companion objects, if there is also a hand-written tupled method.
  def testMapToLocalExplicitTupled = {
    object PersonLocal {
      def tupled(t: (Long, String)): PersonLocal = ???
    }
    case class PersonLocal(id: Long, name: String)
    class People(tag: Tag) extends Table[PersonLocal](tag, "people") {
      def id = column[Long]("id", O.PrimaryKey, O.AutoInc)
      def name = column[String]("name")
      def * = (id, name).mapTo[PersonLocal]
    }
  }

  // This test should simply be able to compile. The mapTo macro should work even when there is
  // a hand-written and overloaded tupled method.
  def testMapToLocalOverloadedTupled = {
    object PersonLocal {
      def tupled(t: (Long, String)): PersonLocal = ???
      def tupled(t: (Long, Int, Char)): PersonLocal = ???
    }
    case class PersonLocal(id: Long, name: String)
    class People(tag: Tag) extends Table[PersonLocal](tag, "people") {
      def id = column[Long]("id", O.PrimaryKey, O.AutoInc)
      def name = column[String]("name")
      def * = (id, name).mapTo[PersonLocal]
    }
  }


  def testMapToLocalBadTupled = {
    object PersonLocal {
      def tupled(t: (Long, Int)): PersonLocal = ???
    }
    case class PersonLocal(id: Long, name: String)
    class People(tag: Tag) extends Table[PersonLocal](tag, "people") {
      def id = column[Long]("id", O.PrimaryKey, O.AutoInc)
      def name = column[String]("name")
      def * = (id, name).mapTo[PersonLocal]
    }
  }

  def testMapToLocal = {
    object PersonLocal {}
    case class PersonLocal(id: Long, name: String)
    class People(tag: Tag) extends Table[PersonLocal](tag, "people") {
      def id = column[Long]("id", O.PrimaryKey, O.AutoInc)
      def name = column[String]("name")
      def * = (id, name).mapTo[PersonLocal]
    }
  }
}
