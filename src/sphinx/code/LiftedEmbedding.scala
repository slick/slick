package com.typesafe.slick.docsnippets

import scala.slick.driver.H2Driver.simple._
import Database.dynamicSession
import java.sql.Date

object LiftedEmbedding extends App {
  // Simple Coffees for Rep types comparison
  {
  //#reptypes
  class Coffees(tag: Tag) extends Table[(String, Double)](tag, "COFFEES") {
    def name = column[String]("COF_NAME")
    def price = column[Double]("PRICE")
    def * = (name, price)
  }
  val coffees = TableQuery[Coffees]

  //#reptypes
  }

  {
//#plaintypes
  case class Coffee(name: String, price: Double)
  val coffees: List[Coffee] = //...

//#plaintypes
    Nil
//#plaintypes
  val l = coffees.filter(_.price > 8.0).map(_.name)
  //                       ^       ^          ^
  //                       Double  Double     String
//#plaintypes

  }
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
//#foreignkey
  class Coffees(tag: Tag) extends Table[(String, Int, Double, Int, Int)](tag, "COFFEES") {
//#foreignkey
    def name = column[String]("COF_NAME", O.PrimaryKey)
//#foreignkey
    def supID = column[Int]("SUP_ID")
//#foreignkey
    def price = column[Double]("PRICE")
//#foreignkey
//#tabledef
    //...
//#tabledef
//#foreignkey
    def sales = column[Int]("SALES", O.Default(0))
    def total = column[Int]("TOTAL", O.Default(0))
    def * = (name, supID, price, sales, total)
//#tabledef
//#foreignkeynav
//#foreignkey
    def supplier = foreignKey("SUP_FK", supID, suppliers)(_.id)
//#foreignkeynav
    // compiles to SQL:
    //   alter table "COFFEES" add constraint "SUP_FK" foreign key("SUP_ID")
    //     references "SUPPLIERS"("SUP_ID")
    //     on update NO ACTION on delete NO ACTION
//#foreignkeynav
//#foreignkey
    def supplier2 = suppliers.filter(_.id === supID)
//#foreignkeynav
//#foreignkey
//#tabledef
  }
//#tabledef
//#tablequery
  val coffees = TableQuery[Coffees]
//#tablequery
//#foreignkey

  {
  //#schemaname
  class Coffees(tag: Tag)
    extends Table[(String, Int, Double, Int, Int)](tag, Some("MYSCHEMA"), "COFFEES") {
    //...
  //#schemaname
    def * = ???
    def name = column[String]("NAME")
  //#schemaname
  }
  //#schemaname

  //#tablequery2
  object coffees extends TableQuery(new Coffees(_)) {
    val findByName = this.findBy(_.name)
  }
  //#tablequery2
  }
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
    // compiles to SQL:
    //   alter table "a" add constraint "pk_a" primary key("k1","k2")
//#primarykey
//#index
    def idx = index("idx_a", (k1, k2), unique = true)
    // compiles to SQL:
    //   create unique index "idx_a" on "a" ("k1","k2")
//#primarykey
  }
//#primarykey
//#index

  val db: Database = Database.forURL("jdbc:h2:mem:test1", driver = "org.h2.Driver")
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
  TableQuery[A].ddl.createStatements.foreach(println)

  db withDynSession {
    //#filtering
    val q1 = coffees.filter(_.supID === 101)
    // compiles to SQL (simplified):
    //   select "COF_NAME", "SUP_ID", "PRICE", "SALES", "TOTAL"
    //     from "COFFEES"
    //     where "SUP_ID" = 101

    val q2 = coffees.drop(10).take(5)
    // compiles to SQL (simplified):
    //   select "COF_NAME", "SUP_ID", "PRICE", "SALES", "TOTAL"
    //     from "COFFEES"
    //     limit 5 offset 10

    val q3 = coffees.sortBy(_.name.desc.nullsFirst)
    // compiles to SQL (simplified):
    //   select "COF_NAME", "SUP_ID", "PRICE", "SALES", "TOTAL"
    //     from "COFFEES"
    //     order by "COF_NAME" desc nulls first
    //#filtering
    println(q1.selectStatement)
    println(q2.selectStatement)
    println(q3.selectStatement)
  }

  db withDynSession {
    //#aggregation1
    val q = coffees.map(_.price)

    val q1 = q.min
    // compiles to SQL (simplified):
    //   select min(x4."PRICE") from "COFFEES" x4

    val q2 = q.max
    // compiles to SQL (simplified):
    //   select max(x4."PRICE") from "COFFEES" x4

    val q3 = q.sum
    // compiles to SQL (simplified):
    //   select sum(x4."PRICE") from "COFFEES" x4

    val q4 = q.avg
    // compiles to SQL (simplified):
    //   select avg(x4."PRICE") from "COFFEES" x4
    //#aggregation1
    println(q.selectStatement)
    println(q1.shaped.selectStatement)
    println(q2.shaped.selectStatement)
    println(q3.shaped.selectStatement)
    println(q4.shaped.selectStatement)
  }

  db withDynSession {
    ddl.create
    //#aggregation2
    val q1 = coffees.length
    // compiles to SQL (simplified):
    //   select count(1) from "COFFEES"

    val q2 = coffees.exists
    // compiles to SQL (simplified):
    //   select exists(select * from "COFFEES")
    //#aggregation2
    println(q1.shaped.selectStatement)
    println(q2.shaped.selectStatement)

    {
      //#invoker
      val l = q.list
      val v = q.buildColl[Vector]
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
    // compiles to SQL:
    //   select x2."SUP_ID", count(1), avg(x2."PRICE")
    //     from "COFFEES" x2, "SUPPLIERS" x3
    //     where x3."SUP_ID" = x2."SUP_ID"
    //     group by x2."SUP_ID"
    //#aggregation3
    println(q2.selectStatement)
  }

  db withDynSession {
    ddl.create
    suppliers ++= Seq(
      (101, "", "", "", "", ""),
      (49, "", "", "", "", ""),
      (150, "", "", "", "", "")
    )

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

    // compiles to SQL:
    //   INSERT INTO "COFFEES" ("COF_NAME","SUP_ID","PRICE","SALES","TOTAL") VALUES (?,?,?,?,?)
    //#insert1
    println(statement)

    users.ddl.create

    //#insert3
    val userId =
      (users returning users.map(_.id)) += User(None, "Stefan", "Zeiger")
    //#insert3
    println((users returning users.map(_.id)).insertStatement)

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
    suppliers.ddl.create
    coffees.ddl.create
    suppliers += (101, "", "", "", "", "")
    coffees += ("Espresso", 101, 0, 0, 0)
    //#update1
    val q = for { c <- coffees if c.name === "Espresso" } yield c.price
    q.update(10.49)

    val statement = q.updateStatement
    val invoker = q.updateInvoker

    // compiles to SQL:
    //   update "COFFEES" set "PRICE" = ? where "COFFEES"."COF_NAME" = 'Espresso'
    //#update1
    println(statement)
  }

  db withDynSession {
    users.ddl.create
    usersForInsert ++= Seq(
      User(None,"",""),
      User(None,"","")
    )

    {
      //#compiled1
      def userNameByIDRange(min: Column[Int], max: Column[Int]) =
        for {
          u <- users if u.id >= min && u.id < max
        } yield u.first

      val userNameByIDRangeCompiled = Compiled(userNameByIDRange _)

      // The query will be compiled only once:
      val names1 = userNameByIDRangeCompiled(2, 5).run
      val names2 = userNameByIDRangeCompiled(1, 3).run
      // Also works for .update and .delete
      //#compiled1
    }

    {
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

  db withDynSession {
    //#mappedtype2
    // A custom ID type for a table
    case class MyID(value: Long) extends MappedTo[Long]

    // Use it directly for this table's ID -- No extra boilerplate needed
    class MyTable(tag: Tag) extends Table[(MyID, String)](tag, "MY_TABLE") {
      def id = column[MyID]("ID")
      def data = column[String]("DATA")
      def * = (id, data)
    }
    //#mappedtype2
  }

  db withDynSession {
    //#recordtypepair
    // A custom record class
    case class Pair[A, B](a: A, b: B)
    //#recordtypepair

    //#recordtype1
    // A Shape implementation for Pair
    final class PairShape[Level <: ShapeLevel, M <: Pair[_,_], U <: Pair[_,_], P <: Pair[_,_]](
      val shapes: Seq[Shape[_, _, _, _]])
    extends MappedScalaProductShape[Level, Pair[_,_], M, U, P] {
      def buildValue(elems: IndexedSeq[Any]) = Pair(elems(0), elems(1))
      def copy(shapes: Seq[Shape[_, _, _, _]]) = new PairShape(shapes)
    }

    implicit def pairShape[Level <: ShapeLevel, M1, M2, U1, U2, P1, P2](
      implicit s1: Shape[_ <: Level, M1, U1, P1], s2: Shape[_ <: Level, M2, U2, P2]
    ) = new PairShape[Level, Pair[M1, M2], Pair[U1, U2], Pair[P1, P2]](Seq(s1, s2))
    //#recordtype1

    //#recordtype2
    // Use it in a table definition
    class A(tag: Tag) extends Table[Pair[Int, String]](tag, "shape_a") {
      def id = column[Int]("id", O.PrimaryKey)
      def s = column[String]("s")
      def * = Pair(id, s)
    }
    val as = TableQuery[A]
    as.ddl.create

    // Insert data with the custom shape
    as += Pair(1, "a")
    as += Pair(2, "c")
    as += Pair(3, "b")

    // Use it for returning data from a query
    val q2 = as
      .map { case a => Pair(a.id, (a.s ++ a.s)) }
      .filter { case Pair(id, _) => id =!= 1 }
      .sortBy { case Pair(_, ss) => ss }
      .map { case Pair(id, ss) => Pair(id, Pair(42 , ss)) }
    //#recordtype2
  }
}
