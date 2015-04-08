package slick.compiler

import slick.ast._

/** Ensure that all collection operations are wrapped in a Bind so that we
  * have a place for expanding references later. FilteredQueries are allowed
  * on top of collection operations without a Bind in between, unless that
  * operation is a Join or a Pure node. */
class ForceOuterBinds extends Phase {
  val name = "forceOuterBinds"

  def apply(state: CompilerState): CompilerState = state.map(apply)

  def apply(n: Node): Node = {
    val t = n.nodeType.structuralRec
    val n2 =
      if(t != UnassignedType && !t.isInstanceOf[CollectionType]) First(wrap(Pure(n)))
      else wrap(n)
    n2.nodeWithComputedType(SymbolScope.empty, typeChildren = true, retype = false)
  }

  def idBind(n: Node): Bind = {
    val gen = new AnonSymbol
    logger.debug("Introducing new Bind "+gen+" for "+n)
    n match {
      case p: Pure => Bind(gen, Pure(ProductNode(Seq())), p)
      case _ => Bind(gen, n, Pure(Ref(gen)))
    }
  }

  def wrap(n: Node): Node = n match {
    case b @ Bind(_, _, Pure(_, _)) => b.nodeMapChildren { ch =>
      if((ch eq b.from) || ch.isInstanceOf[Pure]) nowrap(ch) else maybewrap(ch)
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
    case Path(path) => Path(path) // recreate untyped copy
    case n => n.nodeMapChildren(maybewrap)
  }

  def maybewrap(n: Node): Node = n match {
    case _: Join | _: Pure | _: Union | _: FilteredQuery | _:TableNode => wrap(n)
    case _ => nowrap(n)
  }
}
