package slick.compiler

import slick.SlickTreeException
import slick.ast.Util._
import slick.ast._

/** Verify that all monadic joins have been transformed into applicative joins and that the
  * resulting tree does not contain references to unreachable symbols. */
class VerifySymbols extends Phase {
  val name = "verifySymbols"

  def apply(state: CompilerState) = state.map { n2 =>
    def verifyScoping(n: Node, syms: Set[Symbol]): Unit = n match {
      case FwdPath(s :: _) if !syms.contains(s) =>
        val all = n2.collectAll[(Symbol, Node)] { case d: DefNode => d.nodeGenerators }.toMap
        val srcDef = all.getOrElse(s, null)
        throw new SlickTreeException("Unreachable reference to "+s+" after resolving monadic joins", n2, mark = (d => (d eq n) || (d eq srcDef)))
      case Bind(s, from, sel: Pure) =>
        verifyScoping(from, syms)
        verifyScoping(sel, syms + s)
      case Aggregate(s, from, sel) =>
        verifyScoping(from, syms)
        verifyScoping(sel, syms + s)
      case b @ Bind(s, _, sel) =>
        throw new SlickTreeException("Unresolved monadic join: Non-Pure select clause in Bind "+s, b, mark = (_ eq sel))
      case f: FilteredQuery =>
        verifyScoping(f.from, syms)
        val chSyms = syms + f.nodeGenerators.head._1
        f.nodeChildren.tail.foreach(ch => verifyScoping(ch, chSyms))
      case GroupBy(s, from, by, _) =>
        verifyScoping(from, syms)
        verifyScoping(by, syms + s)
      case Join(s1, s2, f1, f2, _, on) =>
        verifyScoping(f1, syms)
        verifyScoping(f2, syms)
        verifyScoping(on, syms + s1 + s2)
      case n =>
        n.nodeChildren.foreach(ch => verifyScoping(ch, syms))
    }
    verifyScoping(n2, Set.empty)
    n2
  }
}
