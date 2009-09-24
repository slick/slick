package test

import com.novocode.squery.combinator._
import com.novocode.squery.combinator.TypeMapper._
import com.novocode.squery.combinator.extended.H2Driver.Implicit._
import com.novocode.squery.session._
import com.novocode.squery.session.Database.threadLocalSession

object MutateTest {
  def main(args: Array[String]) {

    object Users extends Table[(Option[Int],String,String)]("users") {
      def id = column[Int]("id", O AutoInc, O NotNull, O PrimaryKey)
      def first = column[String]("first", O NotNull)
      def last = column[String]("last", O NotNull)
      def * = id.? ~ first ~ last
    }

    Database.forURL("jdbc:h2:mem:test1", driver = "org.h2.Driver") withSession {

      Users.createTable
      Users.first ~ Users.last insertAll(
        ("Marge", "Bouvier"),
        ("Homer", "Simpson"),
        ("Bart", "Simpson"),
        ("Carl", "Carlson")
      )

      println("Before mutating:")
      Query(Users).foreach(u => println("  "+u))

      val q1 = for(u <- Users if u.last.is("Simpson") || u.last.is("Bouvier")) yield u
      q1.mutate { m =>
        if(m()._3 == "Bouvier") m() = m().copy(_3 = "Simpson")
        else if(m()._2 == "Homer") m.delete()
        else if(m()._2 == "Bart") m.insert((None, "Lisa", "Simpson"))
      }

      println("After mutating:")
      Query(Users).foreach(u => println("  "+u))
    }
  }
}
