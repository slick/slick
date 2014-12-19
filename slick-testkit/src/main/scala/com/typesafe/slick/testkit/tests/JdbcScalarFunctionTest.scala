package com.typesafe.slick.testkit.tests

import java.sql.{Time, Date, Timestamp}
import com.typesafe.slick.testkit.util.{JdbcTestDB, AsyncTest}

class JdbcScalarFunctionTest extends AsyncTest[JdbcTestDB] {
  import tdb.profile.api._

  def test = {
    def check[T](q: Rep[T], exp: T) = q.result.map(_ shouldBe exp)
    def checkLit[T : ColumnType](v: T) = check(LiteralColumn(v), v)

    seq(
      checkLit(Date.valueOf("2011-07-15")),
      checkLit(Time.valueOf("15:53:21")),
      checkLit(Timestamp.valueOf("2011-07-15 15:53:21")),
      { val myExpr = SimpleExpression.binary[Int, Int, Int] { (l, r, qb) =>
          qb.sqlBuilder += '('
          qb.expr(l)
          qb.sqlBuilder += '+'
          qb.expr(r)
          qb.sqlBuilder += "+1)"
        }
        check(myExpr(4, 5), 10) }
    )
  }
}
