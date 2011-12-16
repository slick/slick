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
    val g = f.orElse(pfidentity[Node])
    val memo = new collection.mutable.HashMap[Node, Node]
    def tr(n: Node): Node = {
      memo.getOrElseUpdate(n, {
        g(g(n).nodeMapChildren(tr))
      })
    }
    tr(tree)
  }

  def collectReplace(tree: Node)(pf: PartialFunction[Node, (Node, Node)]): Node = {
    val m = collect(tree, pf).toMap
    val recur = m.andThen(v => m.orElse(pfidentity[Node])(v))
    replace(tree, recur)
  }

  /**
   * Eliminate unnecessary nodes from the AST.
   */
  def eliminateIndirections(tree: Node): Node = {
    val ic = IdContext(tree)
    collectReplace(tree) {
      // Remove alias if aliased node is not referenced otherwise
      case a @ Alias(ch) if ic.checkIdFor(ch).isEmpty => (a, ch)
      // Remove alias if alias is not referenced otherwise
      case a @ Alias(ch) if ic.checkIdFor(a).isEmpty => (a, ch)
      // Remove wrapping of the entire result of a FilteredQuery
      case w @ Wrapped(q @ FilteredQuery(from), what) if what == from => (w, q)
      // Remove dual wrapping (remnant of a removed Filter(_, ConstColumn(true)))
      case w @ Wrapped(in2, w2 @ Wrapped(in1, what)) if in1 == in2 => (w, w2)
      // Remove identity binds
      case b @ Bind(x, Pure(y)) if x == y => (b, x)
      // Remove unnecessary wrapping of pure values
      case p @ Pure(n) => (Wrapped(p, n), p)
      // Remove unnecessary wrapping of binds
      case b @ Bind(_, select) => (Wrapped(b, select), b)
    }
  }

  /**
   * Since Nodes cannot be encoded into Scala Tuples, they have to be encoded
   * into the Tuples' children instead, so we end up with trees like
   * ProductNode(Wrapped(p, x), Wrapped(p, y)). This optimizer rewrites those
   * forms to Wrapped(p, ProductNode(x, y)).
   */
  def reverseProjectionWrapping(tree: Node): Node = {
    def allWrapped(in: Node, xs: Seq[Node]) = xs.forall(_ match {
      case Wrapped(in2, _) if in == in2 => true
      case _ => false
    })
    lazy val pf: PartialFunction[Node, (Node, Node)] = {
      case p @ ProductNode(Wrapped(in, _), xs @ _*) if allWrapped(in, xs) =>
        val newp = ProductNode(p.nodeChildren.collect { case Wrapped(_, what) => what })
        val newp2 = if(pf.isDefinedAt(newp)) pf(newp)._2 else newp
        (p, Wrapped(in, newp2))
    }
    collectReplace(tree)(pf)
  }
}
