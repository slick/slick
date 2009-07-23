package test

import com.novocode.squery.combinator._
import com.novocode.squery.combinator.Implicit._
import com.novocode.squery.session._
import com.novocode.squery.session.SessionFactory._
import com.novocode.squery.session.TypeMapper._

object UnionTest {
  def main(args: Array[String]) {

    object Managers extends Table[(Int, String, String)]("managers") {
      def id = column[Int]("id")
      def name = column[String]("name")
      def department = column[String]("department")
      def * = id ~ name ~ department
    }

    object Employees extends Table[(Int, String, Int)]("employees") {
      def id = column[Int]("id")
      def name = column[String]("name")
      def manager = column[Int]("manager", O.NotNull)
      def * = id ~ name ~ manager

      // A convenience method for selecting employees by department
      def departmentIs(dept: String) = manager in Managers.where(_.department is dept).map(_.id)
    }

    val sf = new DriverManagerSessionFactory("jdbc:h2:mem:test1", "org.h2.Driver")
    sf withSession {

      Managers.createTable
      Employees.createTable

      Managers.insertAll(
        (1, "Peter", "HR"),
        (2, "Amy", "IT"),
        (3, "Steve", "IT")
      )

      Employees.insertAll(
        (4, "Jennifer", 1),
        (5, "Tom", 1),
        (6, "Leonard", 2),
        (7, "Ben", 2),
        (8, "Greg", 3)
      )

      val q1 = for(m <- Managers where { _.department is "IT" }) yield m.id ~ m.name
      println("Managers in IT: "+ q1.selectStatement)
      q1.foreach(o => println("  "+o))

      val q2 = for(e <- Employees where { _.departmentIs("IT") }) yield e.id ~ e.name
      println("Employees in IT: " + q2.selectStatement)
      q2.foreach(o => println("  "+o))

      val q3 = for(x <- q1 union q2; _ <- OrderBy +x._2) yield x
      q3.dump("q3: ")
      println()
      println("Combined and sorted: " + q3.selectStatement)
      q3.foreach(o => println("  "+o))
    }
  }
}
