package com.typesafe.slick.docs

//#imports
import java.sql.Date

import scala.concurrent.{Await, Future}
import scala.concurrent.duration.Duration
import scala.reflect.ClassTag

import slick.jdbc.H2Profile.api.*
//#imports

object LiftedEmbedding {
  def main(args: Array[String]) = {
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
      def supplier = foreignKey("SUP_FK", supID, suppliers)(_.id, onUpdate = ForeignKeyAction.Restrict, onDelete = ForeignKeyAction.Cascade)
      //#foreignkeynav
      // compiles to SQL:
      //   alter table "COFFEES" add constraint "SUP_FK" foreign key("SUP_ID")
      //     references "SUPPLIERS"("SUP_ID")
      //     on update RESTRICT on delete CASCADE
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
      def * = (id.?, first, last) <> ((User.apply _).tupled, User.unapply _)
    }
    val users = TableQuery[Users]
    //#mappedtable
    def usersForInsert = users.map(u => (u.first, u.last).shaped <>
      ( { t => User(None, t._1, t._2) }, { (u: User) => Some((u.first, u.last)) }))
    //#insert2

    //#maptotable
    case class Person(id: Option[Int], first: String, last: String)

    class People(tag: Tag) extends Table[Person](tag, "person") {
      def id = column[Int]("id", O.PrimaryKey, O.AutoInc)
      def first = column[String]("first")
      def last = column[String]("last")
      def * = (id.?, first, last).mapTo[Person]
    }
    val people = TableQuery[People]
    //#maptotable

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

    val db: Database = Database.forConfig("h2mem1")
    try {
      //#ddl
      val schema = coffees.schema ++ suppliers.schema
      //#ddl
      Await.result(
        //#ddl
        db.run(DBIO.seq(
          schema.create,
          schema.createIfNotExists,
          //...
          schema.drop,
          schema.dropIfExists
        ))
        //#ddl
        , Duration.Inf)

      //#ddl2
      schema.create.statements.foreach(println)
      schema.createIfNotExists.statements.foreach(println)
      schema.truncate.statements.foreach(println)
      schema.drop.statements.foreach(println)
      schema.dropIfExists.statements.foreach(println)
      //#ddl2
      TableQuery[A].schema.create.statements.foreach(println);
      {
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

        // building criteria using a "dynamic filter" e.g. from a webform.
        val criteriaColombian = Option("Colombian")
        val criteriaEspresso = Option("Espresso")
        val criteriaRoast: Option[String] = None

        val q4 = coffees.filter { coffee =>
          List(
            criteriaColombian.map(coffee.name === _),
            criteriaEspresso.map(coffee.name === _),
            criteriaRoast.map(coffee.name === _) // not a condition as `criteriaRoast` evaluates to `None`
          ).collect({ case Some(criteria) => criteria }).reduceLeftOption(_ || _).getOrElse(true: Rep[Boolean])
        }
        // compiles to SQL (simplified):
        //   select "COF_NAME", "SUP_ID", "PRICE", "SALES", "TOTAL"
        //     from "COFFEES"
        //     where ("COF_NAME" = 'Colombian' or "COF_NAME" = 'Espresso')

        // Conditional filtering with option e.g. from a webform.
        val optionFromPrice = Option(20.0)
        val optionToPrice: Option[Double] = None

        val q5 = coffees
          .filterOpt(optionFromPrice)(_.price > _)
          .filterOpt(optionToPrice)(_.price < _) // won't be added as a condition as `optionToPrice` evaluates to `None`
        // compiles to SQL (simplified):
        //   select "COF_NAME", "SUP_ID", "PRICE", "SALES", "TOTAL"
        //     from "COFFEES"
        //     where "PRICE" > 20.0

        // Conditional filtering with boolean e.g. from a webform.
        val isRoast = true
        val isEspresso = false

        val q6 = coffees
          .filterIf(isRoast)(_.price > 11.0)
          .filterIf(isEspresso)(_.price > 11.0) // won't be added as a condition as `isEspresso` evaluates to `false`
        // compiles to SQL (simplified):
        //   select "COF_NAME", "SUP_ID", "PRICE", "SALES", "TOTAL"
        //     from "COFFEES"
        //     where "PRICE" > 11.0

        //#filtering
        println(q1.result.statements.head)
        println(q2.result.statements.head)
        println(q3.result.statements.head)
        println(q4.result.statements.head)
        println(q5.result.statements.head)
        println(q6.result.statements.head)
      };
      {
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
        println(q.result.statements.head)
        println(q1.shaped.result.statements.head)
        println(q2.shaped.result.statements.head)
        println(q3.shaped.result.statements.head)
        println(q4.shaped.result.statements.head)
      };
      {
        Await.result(db.run(schema.create), Duration.Inf)
        //#aggregation2
        val q1 = coffees.length
        // compiles to SQL (simplified):
        //   select count(1) from "COFFEES"

        val q2 = coffees.exists
        // compiles to SQL (simplified):
        //   select exists(select * from "COFFEES")
        //#aggregation2
        println(q1.shaped.result.statements.head)
        println(q2.shaped.result.statements.head)

        {
          //#result
          val q = coffees.map(_.price)
          val action = q.result
          val result: Future[Seq[Double]] = db.run(action)
          val sql = action.statements.head
          //#result
          Await.result(result, Duration.Inf)
        }
        {
          //#delete1
          val q = coffees.filter(_.supID === 15)
          val action = q.delete
          val affectedRowsCount: Future[Int] = db.run(action)
          val sql = action.statements.head
          //#delete1
          Await.result(affectedRowsCount, Duration.Inf)
        }
        {
          //#delete2
          //
          val q = coffees filter { coffee =>
            // You can do any subquery here - this example uses the foreign key relation in coffees.
            coffee.supID in (
              coffee.supplier filter {
                _.name === "Delete Me"
              } map {
                _.id
              }
              )
          }
          val action = q.delete
          val affectedRowsCount: Future[Int] = db.run(action)
          val sql = action.statements.head
          //#delete2
          Await.result(affectedRowsCount, Duration.Inf)
        }
      };
      {
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
        println(q2.result.statements.head)
      };
      {
        //#insert1
        val insertActions = DBIO.seq(
          coffees += ("Colombian", 101, 7.99, 0, 0),

          coffees ++= Seq(
            ("French_Roast", 49, 8.99, 0, 0),
            ("Espresso", 150, 9.99, 0, 0)
          ),

          // "sales" and "total" will use the default value 0:
          coffees.map(c => (c.name, c.supID, c.price)) += ("Colombian_Decaf", 101, 8.99)
        )

        // Get the statement without having to specify a value to insert:
        val sql = coffees.insertStatement

        // compiles to SQL:
        //   INSERT INTO "COFFEES" ("COF_NAME","SUP_ID","PRICE","SALES","TOTAL") VALUES (?,?,?,?,?)
        //#insert1
        println(sql)

        Await.result(db.run(DBIO.seq(
          (suppliers ++= Seq(
            (101, "", "", "", "", ""),
            (49, "", "", "", "", ""),
            (150, "", "", "", "", "")
          )),
          insertActions
        )), Duration.Inf)

        //#insert3
        val userId =
          (users returning users.map(_.id)) += User(None, "Stefan", "Zeiger")
        //#insert3
        println((users returning users.map(_.id)).insertStatement)

        //#insert3b
        val userWithId =
          (users returning users.map(_.id)
            into ((user, id) => user.copy(id = Some(id)))
            ) += User(None, "Stefan", "Zeiger")
        //#insert3b
        val userWithIdRes = Await.result(db.run(users.schema.create >> userWithId), Duration.Inf)
        println(userWithIdRes)

        //#insert4
        class Users2(tag: Tag) extends Table[(Int, String)](tag, "users2") {
          def id = column[Int]("id", O.PrimaryKey)
          def name = column[String]("name")
          def * = (id, name)
        }
        val users2 = TableQuery[Users2]

        val actions = DBIO.seq(
          users2.schema.create,
          users2 forceInsertQuery (users.map { u => (u.id, u.first ++ " " ++ u.last) }),
          users2 forceInsertExpr(users.length + 1, "admin")
        )
        //#insert4
        Await.result(db.run(actions), Duration.Inf)

        //#insertOrUpdate
        val updated = users.insertOrUpdate(User(Some(1), "Admin", "Zeiger"))
        // returns: number of rows updated

        val updatedAdmin = (users returning users).insertOrUpdate(User(Some(1), "Slick Admin", "Zeiger"))
        // returns: None if updated, Some((Int, String)) if row inserted
        //#insertOrUpdate
        Await.result(db.run(updated), Duration.Inf)
        Await.result(db.run(updatedAdmin), Duration.Inf)
      };
      {
        //#update1
        val q = for {c <- coffees if c.name === "Espresso"} yield c.price
        val updateAction = q.update(10.49)

        // Get the statement without having to specify an updated value:
        val sql = q.updateStatement

        // compiles to SQL:
        //   update "COFFEES" set "PRICE" = ? where "COFFEES"."COF_NAME" = 'Espresso'
        //#update1
        println(sql)
      };
      {
        //#update2
        val q = coffees.filter(_.name === "Espresso").map(coffee => (coffee.name, coffee.price))
        // A Lungo is more expensive:
        val updateAction = q.update(("Espresso Lungo", 12.88))

        // Get the statement without having to specify an updated value:
        val sql = q.updateStatement

        // compiles to SQL:
        //   update "COFFEES" set "COF_NAME" = ?, "PRICE" = ? where "COFFEES"."COF_NAME" = 'Espresso'
        //#update2
        println(sql)
      };
      {
        Await.result(db.run(
          usersForInsert ++= Seq(
            User(None, "", ""),
            User(None, "", "")
          )
        ), Duration.Inf)

        {
          //#compiled1
          def userNameByIDRange(min: Rep[Int], max: Rep[Int]) =
            for {
              u <- users if u.id >= min && u.id < max
            } yield u.first

          val userNameByIDRangeCompiled = Compiled(userNameByIDRange _)

          // The query will be compiled only once:
          val namesAction1 = userNameByIDRangeCompiled(2, 5).result
          val namesAction2 = userNameByIDRangeCompiled(1, 3).result
          // Also works for .insert, .update and .delete
          //#compiled1
        }

        {
          //#compiled2
          val userPaged = Compiled((d: ConstColumn[Long], t: ConstColumn[Long]) => users.drop(d).take(t))

          val usersAction1 = userPaged(2, 1).result
          val usersAction2 = userPaged(1, 3).result
          //#compiled2
        }

        {
          //#compiled3
          val userCompiled = Compiled(users)
          userCompiled += User(None, "John", "Doe")
          //#compiled3
        }

        {
          //#template1
          val userNameByID = for {
            id <- Parameters[Int]
            u <- users if u.id === id
          } yield u.first

          val nameAction = userNameByID(2).result.head

          val userNameByIDRange = for {
            (min, max) <- Parameters[(Int, Int)]
            u <- users if u.id >= min && u.id < max
          } yield u.first

          val namesAction = userNameByIDRange(2, 5).result
          //#template1
        }
      };
      {
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
        def dayOfWeek2(c: Rep[Date]) =
          SimpleFunction[Int]("day_of_week").apply(Seq(c))
        //#simplefunction2

        assert {
          Await.result(db.run(
            salesPerDay.schema.create >>
              (salesPerDay += ((new Date(999999999), 999))) >> {
              //#simpleliteral
              val current_date = SimpleLiteral[java.sql.Date]("CURRENT_DATE")
              salesPerDay.map(_ => current_date)
              //#simpleliteral
            }.result.head
          ), Duration.Inf).isInstanceOf[java.sql.Date]
        }
      }

      {
        //#mappedtype
        // Custom data type for booleans that maps to NUMBER in database
        object Bool extends Enumeration {
          type Bool = Value
          val True, False = Value

          // A ColumnType that maps it to NUMBER values 1 and 0
          val columnMapper: BaseColumnType[Bool] = MappedColumnType.base[Bool, Int](
            { case True => 1; case False => 0 }, // map Bool to NUMBER
            { i => if (i == 1) True else False } // map NUMBER to Bool
          )
        }

        // Make columnMapper available in table definitions and where you do queries
        implicit val boolColumnType: BaseColumnType[Bool.Bool] = Bool.columnMapper

        // You can now use Bool.{True, False} like any built-in column type (in tables, queries, etc.)
        //#mappedtype
      }

      {
        //#recordtype1
        // A custom record class
        case class Pair[A, B](a: A, b: B)

        // A Shape implementation for Pair
        final class PairShape[Level <: ShapeLevel, M <: Pair[_, _], U <: Pair[_, _] : ClassTag, P <: Pair[_, _]](
                                                                                                                  val shapes: Seq[Shape[_ <: ShapeLevel, _, _, _]])
          extends MappedScalaProductShape[Level, Pair[_, _], M, U, P] {
          def buildValue(elems: IndexedSeq[Any]) = Pair(elems(0), elems(1))
          def copy(shapes: Seq[Shape[_ <: ShapeLevel, _, _, _]]) = new PairShape(shapes)
        }

        implicit def pairShape[Level <: ShapeLevel, M1, M2, U1, U2, P1, P2](
                                                                             implicit s1: Shape[_ <: Level, M1, U1, P1], s2: Shape[_ <: Level, M2, U2, P2]
                                                                           ): PairShape[Level, Pair[M1, M2], Pair[U1, U2], Pair[P1, P2]] = new PairShape[Level, Pair[M1, M2], Pair[U1, U2], Pair[P1, P2]](Seq(s1, s2))
        //#recordtype1

        //#recordtype2
        // Use it in a table definition
        class A(tag: Tag) extends Table[Pair[Int, String]](tag, "shape_a") {
          def id = column[Int]("id", O.PrimaryKey)
          def s = column[String]("s")
          def * = Pair(id, s)
        }
        val as = TableQuery[A]

        // Insert data with the custom shape
        val insertAction = DBIO.seq(
          as += Pair(1, "a"),
          as += Pair(2, "c"),
          as += Pair(3, "b")
        )

        // Use it for returning data from a query
        val q2 = as
          .map { case a => Pair(a.id, (a.s ++ a.s)) }
          .filter { case Pair(id, _) => id =!= 1 }
          .sortBy { case Pair(_, ss) => ss }
          .map { case Pair(id, ss) => Pair(id, Pair(42, ss)) }
        // returns: Vector(Pair(3,Pair(42,"bb")), Pair(2,Pair(42,"cc")))
        //#recordtype2

        assert(Await.result(db.run(as.schema.create >> insertAction >> q2.result), Duration.Inf) == Vector(Pair(3, Pair(42, "bb")), Pair(2, Pair(42, "cc"))))

        //#case-class-shape
        // two custom case class variants
        case class LiftedB(a: Rep[Int], b: Rep[String])
        object LiftedB {
          def tupled = (LiftedB.apply _).tupled
        }
        case class B(a: Int, b: String)

        // custom case class mapping
        implicit object BShape
          extends CaseClassShape[Product, (Rep[Int], Rep[String]), LiftedB, (Int, String), B]((LiftedB.apply _).tupled, (B.apply _).tupled)

        class BRow(tag: Tag) extends Table[B](tag, "shape_b") {
          def id = column[Int]("id", O.PrimaryKey)
          def s = column[String]("s")
          def * = LiftedB(id, s)
        }
        val bs = TableQuery[BRow]

        val insertActions = DBIO.seq(
          bs += B(1, "a"),
          bs.map(b => (b.id, b.s)) += ((2, "c")),
          bs += B(3, "b")
        )

        val q3 = bs
          .map { case b => LiftedB(b.id, (b.s ++ b.s)) }
          .filter { case LiftedB(id, _) => id =!= 1 }
          .sortBy { case LiftedB(_, ss) => ss }

        // returns: Vector(B(3,"bb"), B(2,"cc"))
        //#case-class-shape
        assert(Await.result(db.run(bs.schema.create >> insertActions >> q3.result), Duration.Inf) == Vector(B(3, "bb"), B(2, "cc")))

        //#combining-shapes
        // Combining multiple mapped types
        case class LiftedC(p: Pair[Rep[Int], Rep[String]], b: LiftedB)
        object LiftedC {
          def tupled = (LiftedC.apply _).tupled
        }
        case class C(p: Pair[Int, String], b: B)
        object C {
          def tupled = (C.apply _).tupled
        }

        implicit object CShape
          extends CaseClassShape[Product, (Pair[Rep[Int], Rep[String]], LiftedB), LiftedC, (Pair[Int, String], B), C]((LiftedC.apply _).tupled, (C.apply _).tupled)

        class CRow(tag: Tag) extends Table[C](tag, "shape_c") {
          def id = column[Int]("id")
          def s = column[String]("s")
          def projection = LiftedC(
            Pair(column("p1"), column("p2")), // (cols defined inline, type inferred)
            LiftedB(id, s)
          )
          def * = projection
        }
        val cs = TableQuery[CRow]

        val insertActions2 = DBIO.seq(
          cs += C(Pair(7, "x"), B(1, "a")),
          cs += C(Pair(8, "y"), B(2, "c")),
          cs += C(Pair(9, "z"), B(3, "b"))
        )

        val q4 = cs
          .map { case c => LiftedC(c.projection.p, LiftedB(c.id, (c.s ++ c.s))) }
          .filter { case LiftedC(_, LiftedB(id, _)) => id =!= 1 }
          .sortBy { case LiftedC(Pair(_, p2), LiftedB(_, ss)) => ss ++ p2 }

        // returns: Vector(C(Pair(9,"z"),B(3,"bb")), C(Pair(8,"y"),B(2,"cc")))
        //#combining-shapes
        assert(Await.result(db.run(cs.schema.create >> insertActions2 >> q4.result), Duration.Inf) == Vector(C(Pair(9, "z"), B(3, "bb")), C(Pair(8, "y"), B(2, "cc"))))

        ()
      }
    } finally db.close
  }
}
