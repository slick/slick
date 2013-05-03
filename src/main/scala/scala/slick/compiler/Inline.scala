package scala.slick.compiler

import scala.slick.ast._

/** Inline references to global symbols. */
class Inline extends Phase {
  val name = "inline"

  def apply(state: CompilerState) = state.map {
    case LetDynamic(defs, in) =>
      val globals = defs.toMap
      logger.debug("symbols to inline: "+globals.map(_._1).mkString(", "))
      def tr(n: Node): Node = n match {
        case Ref(a: AnonSymbol) if globals.contains(a) => tr(globals(a))
        case n => n.nodeMapChildren(tr)
      }
      tr(in)
    case n => n
  }
}
