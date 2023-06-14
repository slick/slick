package slick.test.codegen

import slick.jdbc.H2Profile

object CustomTyping {

  import H2Profile.api.*
  sealed trait Bool {
    def isTrue: Boolean
  }
  case object True extends Bool {
    def isTrue = true
  }
  case object False extends Bool {
    def isTrue = false
  }

  implicit val boolTypeMapper: BaseColumnType[Bool] = MappedColumnType.base[Bool, Int](
    { b =>
      if (b == True) 1 else 0
    }, { i =>
      if (i == 1) True else False
    })

  type SimpleA = Tuple2[Bool, String]
  object SimpleA{
    def unapply(s: SimpleA): Option[SimpleA] = Tuple2.unapply(s)
    def apply (s: SimpleA): SimpleA = s
    def tupled(s: SimpleA): SimpleA = s
  }
}
