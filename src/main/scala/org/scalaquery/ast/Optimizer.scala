package org.scalaquery.ast

import collection.mutable.ArrayBuffer
import org.scalaquery.ql.{Pure, Bind, FilteredQuery}

/**
 * Basic optimizers for the ScalaQuery AST
 */
object Optimizer {

  type Optimizer = (Node => Node)

  def pfidentity[T]: PartialFunction[T, T] = { case x => x}

  def collect[T](tree: Node, pf: PartialFunction[Node, T]): Iterable[T] = {
    val b = new ArrayBuffer[T]
    val f = pf.andThen[Unit]{ case t => b += t }.orElse[Node, Unit]{ case _ => () }
    def check(n: Node) {
      f(n)
      n.nodeChildren.foreach(check)
    }
    check(tree)
    b
  }

  def replace(tree: Node, f: PartialFunction[Node, Node]): Node = {
    val seen = new collection.mutable.HashSet[Node]()
    val g = f.orElse(pfidentity[Node])
    val memo = new collection.mutable.HashMap[Node, Node]
    def tr(n: Node): Node = {
      memo.getOrElseUpdate(n, {
        val nn = g(n)
        if(seen(nn)) nn else {
          seen += n
          seen += nn
          nn.nodeMapChildren(tr)
        }
      })
    }
    tr(tree)
  }

  def eliminateIndirections(tree: Node): Node = {
    val ic = IdContext(tree)
    val m = collect[(Node, Node)](tree, {
      // Remove alias if aliased node is not referenced otherwise
      case a @ Alias(ch) if ic.checkIdFor(ch).isEmpty => (a, ch)
      // Remove alias if alias is not referenced otherwise
      case a @ Alias(ch) if ic.checkIdFor(a).isEmpty => (a, ch)
      // Remove wrapping of the entire result of a FilteredQuery
      case w @ Wrapped(what, q @ FilteredQuery(from)) if what eq from => (w, q)
      // Remove dual wrapping (remnant of a removed Filter(_, ConstColumn(true)))
      case w @ Wrapped(w2 @ Wrapped(what, in1), in2) if in1 eq in2 => (w, w2)
      // Remove identity binds
      case b @ Bind(x, Pure(y)) if x == y => (b, x)
    }).toMap
    val recur = m.andThen(v => m.orElse(pfidentity[Node])(v))
    //println("*** "+m)
    replace(tree, recur)
  }
}
