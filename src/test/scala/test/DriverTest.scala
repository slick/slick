package test

import com.novocode.squery.combinator._
import com.novocode.squery.combinator.TypeMapper._
import com.novocode.squery.combinator.extended.{ExtendedProfile, H2Driver, OracleDriver, MySQLDriver}
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
    test(MySQLDriver)
  }

  def test(profile: ExtendedProfile) {
    import profile.Implicit._
    val q1 = Users.where(_.name startsWith "quote ' and backslash \\").take(5)
    println(q1.selectStatement)
    val q2 = Users.where(_.name startsWith "St".bind).drop(10).take(5)
    println(q2.selectStatement)
    val q3 = Query(42 ~ "foo")
    println(q3.selectStatement)
  }
}
