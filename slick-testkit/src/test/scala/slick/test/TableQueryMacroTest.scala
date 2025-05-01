package slick.test

import slick.jdbc.JdbcProfile

// https://github.com/scala/scala3/issues/19933#issuecomment-2816881828
object TableQueryMacroTest {
  case class C(x: Int)

  trait A {
    protected val profile: JdbcProfile

    import profile.api.*

    protected class B(tag: Tag) extends Table[C](tag, "my_table") {
      override def * : slick.lifted.ProvenShape[C] = ???
    }
  }

  trait D extends A {
    import profile.api.*
    val query = TableQuery[B]
  }
}
