package slick.ast

/**
 * The type of a join operation.
 */
abstract class JoinType(val sqlName: String)

object JoinType {
  case object Inner extends JoinType("inner")
  case object Left extends JoinType("left outer")
  case object Right extends JoinType("right outer")
  case object Outer extends JoinType("full outer")
  case object LeftOption extends JoinType("left outer *")
  case object RightOption extends JoinType("right outer *")
  case object OuterOption extends JoinType("full outer *")
  case object Zip extends JoinType("zip")
}
