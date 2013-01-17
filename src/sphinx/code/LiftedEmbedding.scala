package com.typesafe.slick.docsnippets

import scala.slick.driver.H2Driver.simple._
import Database.threadLocalSession
import java.sql.Date

class LiftedEmbedding {

//#foreignkey
  object Suppliers extends Table[(Int, String, String, String, String, String)]("SUPPLIERS") {
    def id = column[Int]("SUP_ID", O.PrimaryKey)
    //...
//#foreignkey
    def name = column[String]("SUP_NAME")
    def street = column[String]("STREET")
    def city = column[String]("CITY")
    def state = column[String]("STATE")
    def zip = column[String]("ZIP")
    // Every table needs a * projection with the same type as the table's type parameter
    def * = id ~ name ~ street ~ city ~ state ~ zip
//#foreignkey
  }

//#foreignkey
//#tabledef
//#reptypes
//#foreignkey
  object Coffees extends Table[(String, Int, Double, Int, Int)]("COFFEES") {
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
    def * = name ~ supID ~ price ~ sales ~ total
//#tabledef
//#foreignkeynav
//#foreignkey
    def supplier = foreignKey("SUP_FK", supID, Suppliers)(_.id)
//#foreignkey
    def supplier2 = Suppliers.where(_.id === supID)
//#foreignkeynav
//#foreignkey
//#tabledef
//#reptypes
  }
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
  val q = Query(Coffees)
  val q2 = q.filter(_.price > 8.0).map(_.name)
  //                  ^       ^          ^
  //          Rep[Double]  Rep[Double]  Rep[String]
//#reptypes

//#mappedtable
//#insert2
  case class User(id: Option[Int], first: String, last: String)

  object Users extends Table[User]("users") {
    def id = column[Int]("id", O.PrimaryKey, O.AutoInc)
    def first = column[String]("first")
    def last = column[String]("last")
    def * = id.? ~ first ~ last <> (User, User.unapply _)
//#mappedtable
    def forInsert = first ~ last <> ({ t => User(None, t._1, t._2)}, { (u: User) => Some((u.first, u.last))})
//#mappedtable
  }
//#mappedtable
//#insert2

//#index
//#primarykey
  object A extends Table[(Int, Int)]("a") {
    def k1 = column[Int]("k1")
    def k2 = column[Int]("k2")
    def * = k1 ~ k2
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
  val ddl = Coffees.ddl ++ Suppliers.ddl
  db withSession {
    ddl.create
    //...
    ddl.drop
  }
//#ddl

//#ddl2
  ddl.createStatements.foreach(println)
  ddl.dropStatements.foreach(println)
//#ddl2

  db withSession {
    //#filtering
    val q = Query(Coffees)
    val q1 = q.filter(_.supID === 101)
    val q2 = q.drop(10).take(5)
    val q3 = q.sortBy(_.name.desc.nullsFirst)
    //#filtering
  }

  db withSession {
    //#aggregation1
    val q = Coffees.map(_.price)
    val q1 = q.min
    val q2 = q.max
    val q3 = q.sum
    val q4 = q.avg
    //#aggregation1
  }

  db withSession {
    //#aggregation2
    val q = Query(Coffees)
    val q1 = q.length
    val q2 = q.exists
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
      //#delete
      val affectedRowsCount = q.delete
      val invoker = q.deleteInvoker
      val statement = q.deleteStatement
      //#delete
    }

    {
      val session = threadLocalSession
      //#invoker_explicit
      val l = q.list(session)
      //#invoker_explicit
    }
  }

  db withSession {
    //#aggregation3
    val q = (for {
      c <- Coffees
      s <- c.supplier
    } yield (c, s)).groupBy(_._1.supID)

    val q2 = q.map { case (supID, css) =>
      (supID, css.length, css.map(_._1.price).avg)
    }
    //#aggregation3
  }

  db withSession {
    //#insert1
    Coffees.insert("Colombian", 101, 7.99, 0, 0)

    Coffees.insertAll(
      ("French_Roast", 49, 8.99, 0, 0),
      ("Espresso",    150, 9.99, 0, 0)
    )

    // "sales" and "total" will use the default value 0:
    (Coffees.name ~ Coffees.supID ~ Coffees.price).insert("Colombian_Decaf", 101, 8.99)

    val statement = Coffees.insertStatement
    val invoker = Coffees.insertInvoker
    //#insert1

  //#insert2

  Users.forInsert insert User(None, "Christopher", "Vogt")
  //#insert2

    //#insert3
    val userId =
      Users.forInsert returning Users.id insert User(None, "Stefan", "Zeiger")
    //#insert3

    //#insert4
    object Users2 extends Table[(Int, String)]("users2") {
      def id = column[Int]("id", O.PrimaryKey)
      def name = column[String]("name")
      def * = id ~ name
    }

    Users2.ddl.create

    Users2 insert (Users.map { u => (u.id, u.first ++ " " ++ u.last) })

    Users2 insertExpr (Query(Users).length + 1, "admin")
    //#insert4
  }

  db withSession {
    //#update1
    val q = for { c <- Coffees if c.name === "Espresso" } yield c.price
    q.update(10.49)

    val statement = q.updateStatement
    val invoker = q.updateInvoker
    //#update1
  }

  db withSession {
    //#template1
    val userNameByID = for {
      id <- Parameters[Int]
      u <- Users if u.id is id
    } yield u.first

    val name = userNameByID(2).first

    val userNameByIDRange = for {
      (min, max) <- Parameters[(Int, Int)]
      u <- Users if u.id >= min && u.id < max
    } yield u.first

    val names = userNameByIDRange(2, 5).list
    //#template1
  }

  db withSession {
    object SalesPerDay extends Table[(Date, Int)]("SALES_PER_DAY") {
      def day = column[Date]("DAY", O.PrimaryKey)
      def count = column[Int]("COUNT")
      def * = day ~ count
    }

    //#simplefunction1
    // H2 has a day_of_week() function which extracts the day of week from a timestamp
    val dayOfWeek = SimpleFunction.unary[Date, Int]("day_of_week")

    // Use the lifted function in a query to group by day of week
    val q1 = for {
      (dow, q) <- SalesPerDay.map(s => (dayOfWeek(s.day), s.count)).groupBy(_._1)
    } yield (dow, q.map(_._2).sum)
    //#simplefunction1

    //#simplefunction2
    def dayOfWeek2(c: Column[Date]) =
      SimpleFunction("day_of_week")(TypeMapper.IntTypeMapper)(Seq(c))
    //#simplefunction2
  }

  db withSession {
    //#mappedtype1
    // An algebraic data type for booleans
    sealed trait Bool
    case object True extends Bool
    case object False extends Bool

    // And a TypeMapper that maps it to Int values 1 and 0
    implicit val boolTypeMapper = MappedTypeMapper.base[Bool, Int](
      { b => if(b == True) 1 else 0 },    // map Bool to Int
      { i => if(i == 1) True else False } // map Int to Bool
    )

    // You can now use Bool like any built-in column type (in tables, queries, etc.)
    //#mappedtype1
  }
}
