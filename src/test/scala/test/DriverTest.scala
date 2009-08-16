package test

import com.novocode.squery.combinator._
import com.novocode.squery.combinator.TypeMapper._
import com.novocode.squery.combinator.extended.{ExtendedProfile, H2Driver, OracleDriver}
import com.novocode.squery.session._
import com.novocode.squery.session.Database._

object DriverTest {
  object Users extends Table[String]("users") {
    def name = column[String]("name", O NotNull)
    def * = name
  }

  def main(args: Array[String]) {
    test(H2Driver)
    test(OracleDriver)
  }

  def test(profile: ExtendedProfile) {
    import profile.Implicit._
    val q1 = Users.where(_.name startsWith "St")
    println(q1.selectStatement)
    val q2 = Users.where(_.name startsWith "St".bind)
    println(q2.selectStatement)
    val q3 = Query(42 ~ "foo")
    println(q3.selectStatement)
  }
}
