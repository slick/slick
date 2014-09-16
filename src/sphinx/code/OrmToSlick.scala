package com.typesafe.slick.docs
import scala.slick.driver.H2Driver.simple._

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

  val jdbcDriver = "org.h2.Driver"  
  val dbUrl = "jdbc:h2:mem:ormtoslick;DB_CLOSE_DELAY=-1"

  Database.forURL(dbUrl,driver=jdbcDriver) withSession { implicit s =>
    addresses.ddl.create
    people.ddl.create
      
    import scala.slick.jdbc.StaticQuery.interpolation
    sql"ALTER TABLE PERSON ALTER COLUMN NAME VARCHAR(255) DEFAULT('')".as[Int].list
    sql"ALTER TABLE PERSON ALTER COLUMN AGE INT DEFAULT(-1)".as[Int].list
    sql"ALTER TABLE PERSON ALTER COLUMN ADDRESS_ID INT DEFAULT(1)".as[Int].list

    SqlToSlick.inserts

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
      val addresses: Seq[Address] = addressesQuery.run
      //#slickExecution
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
            ((p.age: Column[Int]) < 5 || p.age > 65)
            : Column[Boolean]
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
      //#slickRun
      people.filter(_.id === 5).run
      //#slickRun
    };{
      //#ormWriteCaching
      val person = PeopleFinder.getById(5)
      //#ormWriteCaching
    };{
      import scala.language.reflectiveCalls
      val person = new {
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
      personQuery.map(p => (p.name,p.age)).update("C. Vogt",12345)
      //#slickUpdate

      //#slickDelete
      personQuery.delete // deletes person with id 5
      //#slickDelete
    };{
      //#slickInsert
      people.map(p => (p.name,p.age)).insert("S. Zeiger",54321)
      //#slickInsert
    };{
      import scala.language.higherKinds
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

      val chrisWithAddress: (Person, Address) = chrisQuery.withAddress.first
      val stefanWithAddress: (Person, Address) = stefanQuery.withAddress.first
      //#slickRelationships
      };{
        //#relationshipNavigation
        val chris: Person = PeopleFinder.getById(2)
        val address: Address = chris.address
        //#relationshipNavigation
      };{
        /*
        //#relationshipNavigation2
        case class Address( … )
        case class Person( …, address: Address )
        //#relationshipNavigation2
        */
      };{
        //#slickRelationships2
        val chrisQuery: Query[People,Person,Seq] = people.filter(_.id === 2)
        val addressQuery: Query[Addresses,Address,Seq] = chrisQuery.withAddress.map(_._2)
        val address = addressQuery.first
        //#slickRelationships2
      };{
        //#associationTuple
        val tupledJoin: Query[(People,Addresses),(Person,Address), Seq]
              = people join addresses on (_.addressId === _.id)

        case class PersonWithAddress(person: Person, address: Address)
        val caseClassJoinResults = tupledJoin.run map PersonWithAddress.tupled
        //#associationTuple
      }
    }
  }
}
