package test

import com.novocode.squery.combinator._
import com.novocode.squery.combinator.TypeMapper._
import com.novocode.squery.combinator.basic.BasicDriver.Implicit._

object NewOptionMapperTest {
  def main(args: Array[String]) {

    def myLength[T, TM <: TypeMapper[String,T]](c: Column[T])(implicit tm: TM) =
      tm.om(Operator.Length(c))

    val c1:Column[Int] = myLength(new ConstColumn("foo"))

    val c2:Column[Option[Int]] = myLength(new ConstColumn(Some("foo")))

    ()
  }
}
