package scala.slick.compiler

import scala.slick.lifted.Column
import scala.slick.ast._

/** Ensure that all collection operations are wrapped in a Bind so that we
  * have a place for expanding references later. FilteredQueries are allowed
  * on top of collection operations without a Bind in between, unless that
  * operation is a Join or a Pure node. */
class ForceOuterBinds extends Phase {
  val name = "forceOuterBinds"

  def apply(n: Node, state: CompilationState): Node = {
    def idBind(n: Node): Bind = n match {
      case c: Column[_] =>
        idBind(Pure(c))
      case a: Apply =>
        idBind(Pure(a))
      case p: ProductNode =>
        idBind(Pure(p))
      case p: Pure =>
        val gen = new AnonSymbol
        logger.debug("Introducing new Bind "+gen+" for Pure")
        Bind(gen, Pure(ProductNode(Seq())), p)
      case _ =>
        val gen = new AnonSymbol
        logger.debug("Introducing new Bind "+gen)
        Bind(gen, n, Pure(Ref(gen)))
    }
    def wrap(n: Node): Node = n match {
      case b: Bind => b.nodeMapChildren { ch =>
        if((ch eq b.from) || ch.isInstanceOf[Pure]) nowrap(ch)
        else maybewrap(ch)
      }
      case n => idBind(nowrap(n))
    }
    def nowrap(n: Node): Node = n match {
      case u: Union => u.nodeMapChildren(wrap)
      case f: FilteredQuery => f.nodeMapChildren { ch =>
        if((ch eq f.from) && !(ch.isInstanceOf[Join] || ch.isInstanceOf[Pure])) nowrap(ch) else maybewrap(ch)
      }
      case b: Bind => b.nodeMapChildren { ch =>
        if((ch eq b.from) || ch.isInstanceOf[Pure]) nowrap(ch)
        else maybewrap(ch)
      }
      case n => n.nodeMapChildren(maybewrap)
    }
    def maybewrap(n: Node): Node = n match {
      case _: Join => wrap(n)
      case _: Pure => wrap(n)
      case _: Union => wrap(n)
      case _: FilteredQuery => wrap(n)
      case _ => nowrap(n)
    }
    wrap(n)
  }
}
