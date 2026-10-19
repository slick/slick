package slick.compiler

import scala.collection.mutable.ArrayBuffer

import slick.ast.*
import slick.util.ConstArray

/** Flatten nested applications of the associative operators `and` and `or` into a single n-ary
  * application, e.g. `or(or(or(a, b), c), d)` becomes `or(a, b, c, d)`.
  *
  * A chain of conditions built with `reduce(_ || _)` in the lifted embedding is a left-deep binary
  * tree whose depth is the number of conditions. Running this phase first keeps the depth of the
  * tree, and therefore the recursion depth of all later phases and of the code generators,
  * independent of the length of such chains. */
class FlattenBooleanChains extends Phase {
  val name = "flattenBooleanChains"

  def apply(state: CompilerState) = state.map(tr)

  def tr(n: Node): Node = n match {
    case a @ Apply(sym @ (Library.And | Library.Or), ch) =>
      val ops = FlattenBooleanChains.operands(sym, a)
      if(ops.length == ch.length) a.mapChildren(tr, keepType = true)
      else {
        val a2 = Apply(sym, ops.map(tr))(a.buildType)
        if(a.hasType) a2 :@ a.nodeType else a2
      }
    case n => n.mapChildren(tr, keepType = true)
  }
}

object FlattenBooleanChains {
  /** Collect the operands of nested applications of `sym`, in order, without recursion. */
  def operands(sym: TermSymbol, n: Node): ConstArray[Node] = {
    val b = new ArrayBuffer[Node]
    var stack: List[Node] = n :: Nil
    while(stack.nonEmpty) {
      val h = stack.head
      stack = stack.tail
      h match {
        case Apply(s, ch) if s eq sym => stack = ch.toSeq.toList ::: stack
        case _ => b += h
      }
    }
    ConstArray.from(b)
  }
}
