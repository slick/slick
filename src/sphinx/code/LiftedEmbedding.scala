package com.typesafe.slick.docsnippets

import scala.slick.driver.H2Driver.simple._
import Database.dynamicSession
import java.sql.Date

class LiftedEmbedding {

//#foreignkey
  class Suppliers(tag: Tag) extends Table[(Int, String, String, String, String, String)](tag, "SUPPLIERS") {
    def id = column[Int]("SUP_ID", O.PrimaryKey)
    //...
//#foreignkey
    def name = column[String]("SUP_NAME")
    def street = column[String]("STREET")
    def city = column[String]("CITY")
    def state = column[String]("STATE")
    def zip = column[String]("ZIP")
    // Every table needs a * projection with the same type as the table's type parameter
    def * = (id, name, street, city, state, zip)
//#foreignkey
  }
  val suppliers = TableQuery[Suppliers]

//#foreignkey
//#tabledef
//#reptypes
//#foreignkey
  class Coffees(tag: Tag) extends Table[(String, Int, Double, Int, Int)](tag, "COFFEES") {
//#foreignkey
    def name = column[String]("COF_NAME", O.PrimaryKey)
//#reptypes
//#foreignkey
    def supID = column[Int]("SUP_ID")
//#foreignkey
//#reptypes
    def price = column[Double]("PRICE")
//#foreignkey
//#tabledef
    //...
//#tabledef
//#foreignkey
//#reptypes
    def sales = column[Int]("SALES")
    def total = column[Int]("TOTAL")
    def * = (name, supID, price, sales, total)
//#tabledef
//#foreignkeynav
//#foreignkey
    def supplier = foreignKey("SUP_FK", supID, suppliers)(_.id)
//#foreignkey
    def supplier2 = suppliers.filter(_.id === supID)
//#foreignkeynav
//#foreignkey
//#tabledef
//#reptypes
  }
  val coffees = TableQuery[Coffees]
//#foreignkey
//#reptypes
//#tabledef

//#plaintypes
  case class Coffee(name: String, price: Double)
  val l: List[Coffee] = //...
//#plaintypes
    Nil
//#plaintypes
  val l2 = l.filter(_.price > 8.0).map(_.name)
  //                  ^       ^          ^
  //                  Double  Double     String
//#plaintypes

//#reptypes
  val q = coffees.filter(_.price > 8.0).map(_.name)
  //                       ^       ^          ^
  //               Rep[Double]  Rep[Double]  Rep[String]
//#reptypes

//#mappedtable
//#insert2
  case class User(id: Option[Int], first: String, last: String)

  class Users(tag: Tag) extends Table[User](tag, "users") {
    def id = column[Int]("id", O.PrimaryKey, O.AutoInc)
    def first = column[String]("first")
    def last = column[String]("last")
    def * = (id.?, first, last) <> (User.tupled, User.unapply)
  }
  val users = TableQuery[Users]
  //#mappedtable
  def usersForInsert = users.map(u => (u.first, u.last).shaped <>
    ({ t => User(None, t._1, t._2)}, { (u: User) => Some((u.first, u.last))}))
//#insert2

//#index
//#primarykey
  class A(tag: Tag) extends Table[(Int, Int)](tag, "a") {
    def k1 = column[Int]("k1")
    def k2 = column[Int]("k2")
    def * = (k1, k2)
//#index
    def pk = primaryKey("pk_a", (k1, k2))
//#primarykey
//#index
    def idx = index("idx_a", (k1, k2), unique = true)
//#primarykey
  }
//#primarykey
//#index

  val db: Database = null
//#ddl
  val ddl = coffees.ddl ++ suppliers.ddl
  db withDynSession {
    ddl.create
    //...
    ddl.drop
  }
//#ddl

//#ddl2
  ddl.createStatements.foreach(println)
  ddl.dropStatements.foreach(println)
//#ddl2

  db withDynSession {
    //#filtering
    val q1 = coffees.filter(_.supID === 101)
    val q2 = coffees.drop(10).take(5)
    val q3 = coffees.sortBy(_.name.desc.nullsFirst)
    //#filtering
  }

  db withDynSession {
    //#aggregation1
    val q = coffees.map(_.price)
    val q1 = q.min
    val q2 = q.max
    val q3 = q.sum
    val q4 = q.avg
    //#aggregation1
  }

  db withDynSession {
    //#aggregation2
    val q1 = coffees.length
    val q2 = coffees.exists
    //#aggregation2

    {
      //#invoker
      val l = q.list
      val v = q.to[Vector]
      val invoker = q.invoker
      val statement = q.selectStatement
      //#invoker
    }
    {
      val q = coffees
      //#delete
      val affectedRowsCount = q.delete
      val invoker = q.deleteInvoker
      val statement = q.deleteStatement
      //#delete
    }

    {
      val session = dynamicSession
      //#invoker_explicit
      val l = q.list(session)
      //#invoker_explicit
      ()
    }
  }

  db withDynSession {
    //#aggregation3
    val q = (for {
      c <- coffees
      s <- c.supplier
    } yield (c, s)).groupBy(_._1.supID)

    val q2 = q.map { case (supID, css) =>
      (supID, css.length, css.map(_._1.price).avg)
    }
    //#aggregation3
  }

  db withDynSession {
    //#insert1
    coffees += ("Colombian", 101, 7.99, 0, 0)

    coffees ++= Seq(
      ("French_Roast", 49, 8.99, 0, 0),
      ("Espresso",    150, 9.99, 0, 0)
    )

    // "sales" and "total" will use the default value 0:
    coffees.map(c => (c.name, c.supID, c.price)) += ("Colombian_Decaf", 101, 8.99)

    val statement = coffees.insertStatement
    val invoker = coffees.insertInvoker
    //#insert1

  //#insert2

  usersForInsert += User(None, "Christopher", "Vogt")
  //#insert2

    //#insert3
    val userId =
      (usersForInsert returning users.map(_.id)) += User(None, "Stefan", "Zeiger")
    //#insert3

    //#insert4
    class Users2(tag: Tag) extends Table[(Int, String)](tag, "users2") {
      def id = column[Int]("id", O.PrimaryKey)
      def name = column[String]("name")
      def * = (id, name)
    }
    val users2 = TableQuery[Users2]

    users2.ddl.create

    users2 insert (users.map { u => (u.id, u.first ++ " " ++ u.last) })

    users2 insertExpr (users.length + 1, "admin")
    //#insert4
  }

  db withDynSession {
    //#update1
    val q = for { c <- coffees if c.name === "Espresso" } yield c.price
    q.update(10.49)

    val statement = q.updateStatement
    val invoker = q.updateInvoker
    //#update1
  }

  db withDynSession {
    //#template1
    val userNameByID = for {
      id <- Parameters[Int]
      u <- users if u.id is id
    } yield u.first

    val name = userNameByID(2).first

    val userNameByIDRange = for {
      (min, max) <- Parameters[(Int, Int)]
      u <- users if u.id >= min && u.id < max
    } yield u.first

    val names = userNameByIDRange(2, 5).list
    //#template1
  }

  db withDynSession {
    class SalesPerDay(tag: Tag) extends Table[(Date, Int)](tag, "SALES_PER_DAY") {
      def day = column[Date]("DAY", O.PrimaryKey)
      def count = column[Int]("COUNT")
      def * = (day, count)
    }
    val salesPerDay = TableQuery[SalesPerDay]

    //#simplefunction1
    // H2 has a day_of_week() function which extracts the day of week from a timestamp
    val dayOfWeek = SimpleFunction.unary[Date, Int]("day_of_week")

    // Use the lifted function in a query to group by day of week
    val q1 = for {
      (dow, q) <- salesPerDay.map(s => (dayOfWeek(s.day), s.count)).groupBy(_._1)
    } yield (dow, q.map(_._2).sum)
    //#simplefunction1

    //#simplefunction2
    def dayOfWeek2(c: Column[Date]) =
      SimpleFunction[Int]("day_of_week").apply(Seq(c))
    //#simplefunction2
  }

  db withDynSession {
    //#mappedtype1
    // An algebraic data type for booleans
    sealed trait Bool
    case object True extends Bool
    case object False extends Bool

    // And a ColumnType that maps it to Int values 1 and 0
    implicit val boolColumnType = MappedColumnType.base[Bool, Int](
      { b => if(b == True) 1 else 0 },    // map Bool to Int
      { i => if(i == 1) True else False } // map Int to Bool
    )

    // You can now use Bool like any built-in column type (in tables, queries, etc.)
    //#mappedtype1
  }
}
