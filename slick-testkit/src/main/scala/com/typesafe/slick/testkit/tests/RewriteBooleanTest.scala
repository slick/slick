package com.typesafe.slick.testkit.tests

import com.typesafe.slick.testkit.util.{AsyncTest, RelationalTestDB}

class RewriteBooleanTest extends AsyncTest[RelationalTestDB] {
  import tdb.profile.api._

  def testFakeBoolean = {

    case class Person(id: Int, name: String, age: Int, addressId: Option[Int], isHomeOwner: Boolean)

    class People(tag: Tag) extends Table[Person](tag, "people") {
      def id          = column[Int]        ("id", O.PrimaryKey)
      def name        = column[String]     ("name")
      def age         = column[Int]        ("age")
      def addressId   = column[Option[Int]]("address_id")
      def isHomeowner = column[Boolean]    ("is_homeowner")

      def * = (id, name, age, addressId, isHomeowner).<>((Person.apply _).tupled, Person.unapply _)
    }

    lazy val people = TableQuery[People]

    case class Address(id: Int, stree: String, city: String, isActive: Boolean)

    class Addresses(tag: Tag) extends Table[Address](tag, "addresses") {
      def id       = column[Int]    ("id", O.PrimaryKey)
      def street   = column[String] ("street")
      def city     = column[String] ("city")
      def isActive = column[Boolean]("is_active")

      def * = (id, street, city, isActive).<>((Address.apply _).tupled, Address.unapply _)
    }

    lazy val addresses = TableQuery[Addresses]

    val peopleQuery = {
      val peopleJoin = people.joinLeft(addresses).on(_.addressId === _.id)

      def query(personId: Int) = for {
        (person, address) <- peopleJoin
        if person.id === personId
      } yield (person, address)

      query _
    }

    def getPersonAddressTuple(id: Int) = peopleQuery(id)

    seq(
      (people.schema ++ addresses.schema).create,
      addresses
        .map(r => (r.id, r.street , r.city,  r.isActive))
        .+= (     (   1,  "street",  "city",       true)),
      people
        .map(r => (r.id, r.name, r.age, r.addressId, r.isHomeowner))
        .+= (     (   1,  "name",   21,     Some(1),          true)),
      getPersonAddressTuple(1).result.map(_.size shouldBe 1)
    )

  }

}
