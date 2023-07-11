package com.typesafe.slick.testkit.tests

import com.typesafe.slick.testkit.util.{AsyncTest, JdbcTestDB}

class ForceInsertQueryTest extends AsyncTest[JdbcTestDB] {

  import tdb.profile.api._

  case class Person(id: Option[Long] = None,
                    name: Option[String] = None,
                    hairColor: Option[String] = None,
                    eyeColor: Option[String] = None)

  class People(tag: Tag) extends Table[Person](tag, "people") {
    def id = column[Long]("id", O.PrimaryKey, O.AutoInc)

    def name = column[Option[String]]("name")

    def hairColor = column[Option[String]]("hair_color")

    def eyeColor = column[Option[String]]("eye_color")

    def * = (id.?, name, hairColor, eyeColor).mapTo[Person]
  }

  object peopleTable extends TableQuery(new People(_)) {
    def someForcedInsert(person: Person) = {
      this.map(p => (p.name, p.hairColor, p.eyeColor))
        .forceInsertQuery {
          Query((person.name, person.hairColor, person.eyeColor))
        }
    }
  }

  def testForceInsert = {
    DBIO.seq(
      peopleTable.schema.create,
      peopleTable.someForcedInsert(Person(name = Some("John"), hairColor = Some("Brown"), eyeColor = Some("Brown"))),
      peopleTable.someForcedInsert(Person(name = Some("John"), hairColor = None, eyeColor = None)),
      peopleTable.someForcedInsert(Person(name = None, hairColor = None, eyeColor = None)),
      peopleTable.someForcedInsert(Person(name = Some("John"), hairColor = None, eyeColor = Some("Blue"))),
      peopleTable.schema.drop
    )
  }
}
