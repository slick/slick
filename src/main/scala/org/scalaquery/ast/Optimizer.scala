package org.scalaquery.ast

import org.scalaquery.util.RefId
import collection.mutable.ArrayBuffer
import org.scalaquery.ql.FilteredQuery

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

  def replace(tree: Node, f: PartialFunction[RefId[Node], RefId[Node]]): Node = {
    val seen = new collection.mutable.HashSet[RefId[Node]]()
    val g = f.orElse[RefId[Node], RefId[Node]] { case x => x }
    val memo = new collection.mutable.HashMap[RefId[Node], RefId[Node]]
    def tr(n: Node): Node = {
      val ref = RefId(n)
      memo.getOrElseUpdate(ref, {
        val newRef = g(ref)
        if(seen(newRef)) newRef else {
          seen += ref
          seen += newRef
          RefId(newRef.e.nodeMapChildren(tr))
        }
      }).e
    }
    tr(tree)
  }

  def eliminateIndirections(tree: Node): Node = {
    val ic = IdContext(tree)
    val m = collect[(RefId[Node], RefId[Node])](tree, {
      // Remove alias if aliased node is not referenced otherwise
      case a @ Alias(ch) if ic.checkIdFor(ch).isEmpty => (RefId(a), RefId(ch))
      // Remove alias if alias is not referenced otherwise
      case a @ Alias(ch) if ic.checkIdFor(a).isEmpty => (RefId(a), RefId(ch))
      // Remove wrapping of the entire result of a FilteredQuery
      case w @ Wrapped(what, q @ FilteredQuery(from)) if what eq from => (RefId(w), RefId(q))
      // Remove dual wrapping (remnant of a removed Filter(_, ConstColumn(true)))
      case w @ Wrapped(w2 @ Wrapped(what, in1), in2) if in1 eq in2 => (RefId(w), RefId(w2))
    }).toMap
    val recur = m.andThen(v => m.orElse(pfidentity[RefId[Node]])(v))
    //println("*** "+m)
    replace(tree, recur)
  }
}
