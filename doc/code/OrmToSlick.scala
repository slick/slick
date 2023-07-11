package com.typesafe.slick.docs

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import slick.jdbc.H2Profile.api._

object OrmToSlick extends App {
  import SqlToSlick.Tables._

  // fake ORM
  object PeopleFinder{
    def getByIds(ids: Seq[Int]): Seq[Person] = Seq()
    def getById(id: Int): Person = null
  }
  implicit class OrmPersonAddress(person: Person){
    def address: Address = null
  }
  implicit class OrmPrefetch(people: Seq[Person]){
    def prefetch(f: Person => Address) = people
  }
  object session{
    def createQuery(hql: String) = new HqlQuery
    def createCriteria(cls: java.lang.Class[_]) = new Criteria
    def save = ()
  }
  class Criteria{
    def add(r: Restriction) = this
  }
  type Restriction = Criteria
  class HqlQuery{
    def setParameterList(column: String, values: Array[_]): Unit = ()
  }
  object Property{
    def forName(s:String) = new Property
  }
  class Property{
    def in(array: Array[_]): Restriction = new Restriction
    def lt(i: Int) = new Restriction
    def gt(i: Int) = new Restriction
  }
  object Restrictions{
    def disjunction = new Criteria
  }

  val db = Database.forConfig("h2mem1")
  try {
    val setup = DBIO.seq(
      addresses.schema.create,
      people.schema.create,
      sql"ALTER TABLE PERSON ALTER COLUMN NAME VARCHAR(255) DEFAULT('')".as[Int],
      sql"ALTER TABLE PERSON ALTER COLUMN AGE INT DEFAULT(-1)".as[Int],
      sql"ALTER TABLE PERSON ALTER COLUMN ADDRESS_ID INT DEFAULT(1)".as[Int],
      SqlToSlick.inserts
    )
    Await.result(db.run(setup), Duration.Inf)

    ;{
      //#ormObjectNavigation
      val people: Seq[Person] = PeopleFinder.getByIds(Seq(2,99,17,234))
      val addresses: Seq[Address] = people.map(_.address)
      //#ormObjectNavigation
    };{
      //#ormPrefetch
                                // tell the ORM to load all related addresses at once
      val people: Seq[Person] = PeopleFinder.getByIds(Seq(2,99,17,234)).prefetch(_.address)
      val addresses: Seq[Address] = people.map(_.address)
      //#ormPrefetch
    }

    ;{
      //#slickNavigation
      val peopleQuery: Query[People,Person,Seq] = people.filter(_.id inSet(Set(2,99,17,234)))
      val addressesQuery: Query[Addresses,Address,Seq] = peopleQuery.flatMap(_.address)
      //#slickNavigation
      //#slickExecution
      val addressesAction: DBIO[Seq[Address]] = addressesQuery.result
      val addresses: Future[Seq[Address]] = db.run(addressesAction)
      //#slickExecution
      Await.result(addresses, Duration.Inf)
    };{
      type Query = HqlQuery
      //#hqlQuery
      val hql: String = "FROM Person p WHERE p.id in (:ids)"
      val q: Query = session.createQuery(hql)
      q.setParameterList("ids", Array(2,99,17,234))
      //#hqlQuery
    };{
      //#criteriaQuery
      val id = Property.forName("id")
      val q = session.createCriteria(classOf[Person])
                     .add( id in Array(2,99,17,234) )
      //#criteriaQuery
      //#criteriaQueryComposition
      def byIds(c: Criteria, ids: Array[Int]) = c.add( id in ids )

      val c = byIds(
        session.createCriteria(classOf[Person]),
        Array(2,99,17,234)
      )      
      //#criteriaQueryComposition
    };{
      //#criteriaComposition
      val age = Property.forName("age")
      val q = session.createCriteria(classOf[Person])
                     .add(
                       Restrictions.disjunction
                         .add(age lt 5)
                         .add(age gt 65)
                     )
      //#criteriaComposition
    };{
      //#slickQuery
      val q = people.filter(p => p.age < 5 || p.age > 65)
      //#slickQuery
    };{
      //#slickQueryWithTypes
      val q = (people: Query[People, Person, Seq]).filter(
        (p: People) => 
          (
            ((p.age: Rep[Int]) < 5 || p.age > 65)
            : Rep[Boolean]
          )
      )
      //#slickQueryWithTypes
    };{
      //#slickForComprehension
      for( p <- people if p.age < 5 || p.age > 65 ) yield p
      //#slickForComprehension
    };{
      //#slickOrderBy
      ( for( p <- people if p.age < 5 || p.age > 65 ) yield p ).sortBy(_.name)
      //#slickOrderBy
    };{
      //#slickMap
      people.map(p => (p.name, p.age))
      //#slickMap
    };{
      //#ormGetById
      PeopleFinder.getById(5)
      //#ormGetById
    };{
      Await.result(
        //#slickRun
        db.run(people.filter(_.id === 5).result)
        //#slickRun
      , Duration.Inf)
    };{
      //#ormWriteCaching
      val person = PeopleFinder.getById(5)
      //#ormWriteCaching
    };{
      object person {
        var name: String = ""
        var age: Int = 0
      }
      //#ormWriteCaching
      person.name = "C. Vogt"
      person.age = 12345
      session.save
      //#ormWriteCaching
    };{
      //#slickUpdate
      val personQuery = people.filter(_.id === 5)
      personQuery.map(p => (p.name,p.age)).update("C. Vogt", 12345)
      //#slickUpdate

      //#slickDelete
      personQuery.delete // deletes person with id 5
      //#slickDelete
    };{
      //#slickInsert
      people.map(p => (p.name,p.age)) += ("S. Zeiger", 54321)
      //#slickInsert
    };{
      //#slickRelationships
      implicit class PersonExtensions[C[_]](q: Query[People, Person, C]) {
        // specify mapping of relationship to address
        def withAddress = q.join(addresses).on(_.addressId === _.id)
      }
      //#slickRelationships
      ;{
      //#slickRelationships
      
      val chrisQuery = people.filter(_.id === 2)
      val stefanQuery = people.filter(_.id === 3)

      val chrisWithAddress: Future[(Person, Address)] =
        db.run(chrisQuery.withAddress.result.head)
      val stefanWithAddress: Future[(Person, Address)] =
        db.run(stefanQuery.withAddress.result.head)
      //#slickRelationships
      Await.result(chrisWithAddress, Duration.Inf)
      Await.result(stefanWithAddress, Duration.Inf)
      }
      {
        //#relationshipNavigation
        val chris: Person = PeopleFinder.getById(2)
        val address: Address = chris.address
        //#relationshipNavigation
      }

      /*
      //#relationshipNavigation2
      case class Address( … )
      case class Person( …, address: Address )
      //#relationshipNavigation2
      */

      {
        //#slickRelationships2
        val chrisQuery: Query[People,Person,Seq] = people.filter(_.id === 2)
        val addressQuery: Query[Addresses,Address,Seq] = chrisQuery.withAddress.map(_._2)
        val address = db.run(addressQuery.result.head)
        //#slickRelationships2
        Await.result(address, Duration.Inf)
      };{
        import scala.concurrent.ExecutionContext.Implicits.global
        //#associationTuple
        val tupledJoin: Query[(People,Addresses),(Person,Address), Seq]
              = people join addresses on (_.addressId === _.id)

        case class PersonWithAddress(person: Person, address: Address)
        val caseClassJoinResults = db.run(tupledJoin.result).map(_.map((PersonWithAddress.apply _).tupled))
        //#associationTuple
      }
    }
  } finally db.close
}
