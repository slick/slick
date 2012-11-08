package scala.slick.docsnippets

import scala.slick.driver.H2Driver.simple._
import Database.threadLocalSession

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
  case class User(id: Option[Int], first: String, last: String)

  object Users extends Table[User]("users") {
    def id = column[Int]("id", O.PrimaryKey, O.AutoInc)
    def first = column[String]("first")
    def last = column[String]("last")
    def * = id.? ~ first ~ last <> (User, User.unapply _)
  }
//#mappedtable

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
}
