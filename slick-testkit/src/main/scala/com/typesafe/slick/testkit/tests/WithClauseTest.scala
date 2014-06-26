package com.typesafe.slick.testkit.tests

import com.typesafe.slick.testkit.util.{JdbcTestDB, TestkitTest}
import org.junit.Assert._

import scala.slick.driver.PostgresDriver

class WithClauseTest extends TestkitTest[JdbcTestDB] {

  import tdb.profile.simple._

  override val reuseInstance = true

  class Managers(tag: Tag) extends Table[(Int, String, String)](tag, "managers") {
    def id = column[Int]("id")

    def name = column[String]("name")

    def department = column[String]("department")

    def * = (id, name, department)
  }

  lazy val managers = TableQuery[Managers]

  class Employees(tag: Tag) extends Table[(Int, String, Int)](tag, "employees") {
    def id = column[Int]("id")

    def name = column[String]("name2")

    def manager = column[Int]("manager")

    def * = (id, name, manager)
  }

  lazy val employees = TableQuery[Employees]

  def testBasic(): Unit = {
    if (!tdb.driver.isInstanceOf[PostgresDriver]) return
    val factorial = Query(0, 1).
      map { case (n: Column[Int], fact: Column[Int]) => (n, fact)}.
      recursiveUnion {
      case t =>
        Query(t).
          filter { case (n: Column[Int], fact: Column[Int]) => n < 9}.
          map { case (n: Column[Int], fact: Column[Int]) => (n + 1, (n + 1) * fact)}
    }
    val fibonacci = Query(0, 0, 1, 1).
      map { case (fibA, fibB, seed, num) => (fibA, fibB, seed, num)}.
      recursiveUnion {
      case t => Query(t).
        filter {
        case (fibA: Column[Int], fibB: Column[Int], seed: Column[Int], num: Column[Int]) => num < 12
      }.map {
        case (fibA: Column[Int], fibB: Column[Int], seed: Column[Int], num: Column[Int]) =>
          (seed + fibA, fibB + fibA, fibA, num + 1)
      }
    }
    val resultFibonacci = fibonacci.list
    val resultFactorial = factorial.list
    def fibonacciStream(n: (Int, Int, Int, Int)): Stream[(Int, Int, Int, Int)] =
      Stream.cons(n, fibonacciStream((n._1 + n._3, n._2 + n._1, n._1, n._4 + 1)))
    def factorialStream(n: (Int, Int)): Stream[(Int, Int)] =
      Stream.cons(n, factorialStream((n._1 + 1, (n._1 + 1) * n._2)))
    val expectedFibonacci = fibonacciStream((0, 0, 1, 1)).drop(0).take(resultFibonacci.length).toList
    val expectedFactorial = factorialStream((0, 1)).drop(0).take(resultFactorial.length).toList
    assertEquals(expectedFibonacci, resultFibonacci)
    assertEquals(expectedFactorial, resultFactorial)

    // single column example
    val resultOne = Query(1).map(t => t).recursiveUnion(t => Query(t).filter(t => t < 10).map(t => (t + 1))).list
    val expectedOne = 1.to(10).map(x => Tuple1(x)).toList
    assertEquals(expectedOne, resultOne)
  }

  def testMultiWithClause(): Unit = {
    if (!tdb.driver.isInstanceOf[PostgresDriver]) return
    class Employees(tag: Tag) extends Table[(Int, String, Int)](tag, "emp_multiwithclause") {
      def id = column[Int]("id")

      def name = column[String]("name2")

      def manager = column[Int]("manager")

      def * = (id, name, manager)
    }
    val employees = TableQuery[Employees](new Employees(_))
    employees.ddl.create
    employees.++=(1.to(15).map(x => (x, f"num$x", x + 1)))
    def utilMapper(f: Employees) = (f.id, f.name, f.manager)
    type QueryType = Query[(Column[Int], Column[String], Column[Int]), (Int, String, Int), Seq]
    val withDeclarationOne =
      employees.filter(t => t.id === 1).map(utilMapper).
        recursiveUnion {
        case t => Query(t).asInstanceOf[QueryType].
          join(employees).on {
          case ((id, name, manager), employee) => manager === employee.id
        }.filter {
          case ((id, name, manager), employee) => id < 4
        }.map {
          case ((id, name, manager), employee) => (employee.id, employee.name, employee.manager)
        }.asInstanceOf[QueryType]
      }
    val withDeclarationTwo =
      employees.filter(t => t.id === 6).map(utilMapper).
        recursiveUnion {
        case t => Query(t).asInstanceOf[QueryType].
          join(employees).on {
          case ((id, name, manager), employee) => manager === employee.id
        }.filter {
          case ((id, name, manager), employee) => id < 9
        }.map {
          case ((id, name, manager), employee) => (employee.id, employee.name, employee.manager)
        }.asInstanceOf[QueryType]
      }
    val withDeclarationThree =
      employees.filter(t => t.id === 11).map(utilMapper).
        recursiveUnion {
        case t => Query(t).asInstanceOf[QueryType].
          join(employees).on {
          case ((id, name, manager), employee) => manager === employee.id
        }.filter {
          case ((id, name, manager), employee) => id < 14
        }.map {
          case ((id, name, manager), employee) => (employee.id, employee.name, employee.manager)
        }.asInstanceOf[QueryType]
      }
    val union = withDeclarationOne.union(withDeclarationTwo).union(withDeclarationThree)
    val qOne = union.sortBy(t => t._1)
    val result = qOne.list
    val expected = 1.to(15).
      filter(x => (1 <= x && x <= 4) || (6 <= x && x <= 9) || (11 <= x && x <= 14)).
      map(x => (x, f"num$x", x + 1))
    assertEquals(expected, result)
    val result2 = union.groupBy(t => ()).map { case (g, t) => (t.map(t => t._1).sum, t.map(t => t._3).sum)}.list
    val expected2 = Seq(
      expected.tail.foldLeft((expected.head._1, expected.head._3)) { case ((a, b), (x, _, z)) => (a + x, b + z)}
    ).map { case (a, b) => (Some(a), Some(b))}
    assertEquals(expected2, result2)
    employees.ddl.drop
  }

  def testMultiWithClause2(): Unit = {
    if (!tdb.driver.isInstanceOf[PostgresDriver]) return
    class Employees(tag: Tag) extends Table[(Int, String, Int)](tag, "emp_multiwithclause2") {
      def id = column[Int]("id")

      def name = column[String]("name2")

      def manager = column[Int]("manager")

      def * = (id, name, manager)
    }
    val employees = TableQuery[Employees](new Employees(_))
    employees.ddl.create
    employees.++=(1.to(15).map(x => (x, f"num$x", x + 1)))
    def utilMapper(f: Employees) = (f.id, f.name, f.manager)
    type QueryType = Query[(Column[Int], Column[String], Column[Int]), (Int, String, Int), Seq]
    type QueryType2 = Query[Column[Int], Int, Seq]
    val withDeclarationOne =
      employees.filter(t => (t.id > 5 && t.id <= 10)).map(t => t.id).
        recursiveUnion {
        case t => Query(t).
          filter {
          case (id) => id < 1
        }.map {
          case (id) => (id)
        }
      }
    val withDeclarationTwo =
      employees.filter(t => (t.id >= 1 && t.id <= 5)).map(t => t.id).
        recursiveUnion {
        case t => Query(t).
          filter {
          case (id) => id < 1
        }.map {
          case (id) => (id)
        }
      }
    val withDeclarationThree =
      employees.filter(t => t.id === 1).map(utilMapper).
        recursiveUnion {
        case t => Query(t).asInstanceOf[QueryType].
          join(employees).on {
          case ((id, name, manager), employees) => manager === employees.id
        }.filter {
          case ((id, name, manager), employees) => id.in(withDeclarationTwo) || id.in(withDeclarationOne)
        }.map {
          case ((id, name, manager), employees) => (employees.id, employees.name, employees.manager)
        }.asInstanceOf[QueryType]
      }

    val result = withDeclarationThree.sortBy(t => (t._1)).list
    val expected = 1.to(11).
      map(x => (x, f"num$x", x + 1))
    assertEquals(expected, result)

    employees.ddl.drop
  }

  def testMoreComplicated(): Unit = {
    if (!tdb.driver.isInstanceOf[PostgresDriver]) return
    case class ModelA(var id: Option[Int],
                      var parentId: Option[Int],
                      var name: Option[String])

    class TableA(tag: Tag) extends Table[ModelA](tag, "model_a") {
      def id = column[Option[Int]]("id", O.PrimaryKey, O.AutoInc)

      def parentId = column[Option[Int]]("parent_id", O.Nullable)

      def name = column[Option[String]]("name", O.Nullable)

      override def * = (id, parentId, name) <>(ModelA.tupled, ModelA.unapply)
    }

    val tableA = TableQuery[TableA](new TableA(_))
    tableA.ddl.create

    type TypeOne = Query[(Column[Option[Int]], Column[Option[Int]]), (Option[Int], Option[Int]), Seq]
    type TypeTwo = Query[
      (Column[Option[Int]], Column[Option[Int]], Column[Option[String]], Column[Option[Int]], Column[Option[Int]]),
      (Option[Int], Option[Int], Option[String], Option[Int], Option[Int]),
      Seq]
    def partOne = {
      val inParameter = tableA.filter(t ⇒ t.parentId.isEmpty).map(t ⇒ t.id)
      val joinOne =
        tableA.
          filter(t ⇒ t.parentId.in(inParameter)).
          groupBy(t ⇒ t.parentId).
          map { case (parentId, t) => (parentId, t.map(t ⇒ t.id).countDistinct.asColumnOf[Option[Int]])}.
          asInstanceOf[TypeOne]
      tableA.
        filter(t ⇒ t.parentId.isEmpty).
        join(joinOne).
        on(_.id === _._1).
        map {
        case (a, (_, c)) =>
          (a.id, a.parentId, a.name,
            LiteralColumn(Option(0)), // level
            c) // subNodeCount
      }
    }

    val partTwo = partOne.
      map { case (id, parentId, name, level, subNodeCount) => (id, parentId, name, level, subNodeCount)}.
      recursiveUnion {
      case (id, parentId, name, level, subNodeCount) =>
        val joinOne =
          tableA.
            groupBy(t ⇒ t.parentId).
            map { case (parentId, t) => (parentId, t.map(t ⇒ t.id).countDistinct.asColumnOf[Option[Int]])}.
            asInstanceOf[TypeOne]
        Query(id, parentId, name, level, subNodeCount).
          asInstanceOf[TypeTwo].
          join(tableA).on {
          case ((id, _, _, _, _), tabA) => id === tabA.parentId
        }.leftJoin(joinOne).on {
          case ((_, tabA), (grpBy, _)) => tabA.id === grpBy
        }.map {
          case (((id, parentId, name, level, subNodeCount), b), (c, d)) =>
            (b.id, b.parentId, b.name,
              (level + 1).asColumnOf[Option[Int]], // level
              Case.If(d.isDefined).Then(d).Else(LiteralColumn(Option(0)))) //subNodeCount
        }
    }.asInstanceOf[TypeTwo].
      sortBy {
      case (id, parentId, name, level, subNodeCount) => (level.asc, id.asc)
    }

    var data = List[ModelA]()
    var expectedData = List[(Option[Int], Option[Int], Option[String], Option[Int], Option[Int])]()
    def numberStreamSeed(i: Int): Stream[Int] = Stream.cons(i, numberStreamSeed(i + 1))
    val numberIterator = numberStreamSeed(1).iterator
    1.to(5).foreach { x1 =>
      val currentLevOne = ModelA(Some(numberIterator.next()), None, Some(f"node$x1"))
      data = currentLevOne :: data
      expectedData = (currentLevOne.id, currentLevOne.parentId, currentLevOne.name, Option(0), Option(4)) :: expectedData
      1.to(4).foreach { x2 =>
        val currentLevTwo = ModelA(Some(numberIterator.next()), currentLevOne.id, Some(f"node$x1-$x2"))
        expectedData = (currentLevTwo.id, currentLevTwo.parentId, currentLevTwo.name, Option(1), Option(3)) :: expectedData
        data = currentLevTwo :: data
        1.to(3).foreach { x3 =>
          val currentLevThree = ModelA(Some(numberIterator.next()), currentLevTwo.id, Some(f"node$x1-$x2-$x3"))
          data = currentLevThree :: data
          expectedData = (currentLevThree.id, currentLevThree.parentId, currentLevThree.name, Option(2), Option(0)) :: expectedData
        }
      }
    }
    tableA.++=(data.reverse)
    val result = partTwo.list
    val expected = expectedData.sortBy { case (a, b, c, d, e) => (d, a)}
    assertEquals(expected, result)

    tableA.ddl.drop
  }
}
