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

  case class SimpleA(_1: Bool, _2: String)
  // We need to use `mapTo` instead of `<>` for Dotty compatibility (because the old `unapply` is gone),
  // so this is no longer supported, at least in 2.x. It does work in Dotty but that is due to `SimpleA` being
  // a tuple, not because of the companion object methods.
  /*
  type SimpleA = Tuple2[Bool, String]
  object SimpleA{
    def unapply(s: SimpleA): Option[SimpleA] = Tuple2.unapply(s)
    def apply (s: SimpleA): SimpleA = s
    def tupled(s: SimpleA): SimpleA = s
  }
   */
}
