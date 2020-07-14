package com.typesafe.slick.testkit.tests

import com.typesafe.slick.testkit.util.{RelationalTestDB, AsyncTest}

import scala.concurrent.Future

class CompanionObjectTest extends AsyncTest[RelationalTestDB] {
  import tdb.profile.api._

  def testMapTo = {
    object Person {}
    case class Person(id: Long, name: String)
    class People(tag: Tag) extends Table[Person](tag, "people") {
      def id = column[Long]("id", O.PrimaryKey, O.AutoInc)
      def name = column[String]("name")
      def * = (id, name).mapTo[Person]
    }
  }
}
