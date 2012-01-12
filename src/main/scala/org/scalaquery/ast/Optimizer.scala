package org.scalaquery.ast

import collection.mutable.HashMap
import OptimizerUtil._
import org.scalaquery.util.RefId
import org.scalaquery.ql._

/**
 * Basic optimizers for the ScalaQuery AST
 */
object Optimizer {

  /**
   * Eliminate unnecessary nodes from the AST.
   */
  def eliminateIndirections = new Transformer {
    def replace = {
      // Remove wrapping of the entire result of a FilteredQuery
      case Wrapped(q @ FilteredQuery(from), what) if what == from => q
      // Remove dual wrapping (remnant of a removed Filter(_, ConstColumn(true)))
      case Wrapped(in2, w2 @ Wrapped(in1, what)) if in1 == in2 => w2
      // Remove identity binds
      case Bind(_, x, Pure(y)) if x == y => x
      // Remove unnecessary wrapping of pure values
      case Wrapped(p @ Pure(n1), n2) if n1 == n2 => p
      // Remove unnecessary wrapping of binds
      case Wrapped(b @ Bind(_, _, s1), s2) if s1 == s2 => b
      // Remove unnecessary wrapping of filters in FROM clauses
      case b @ Bind(gen, Wrapped(f: FilteredQuery[_,_], _), what) => b.copy(from = f)()
      case b @ Bind(gen, Wrapped(f: FilteredJoin[_,_,_,_], _), what) => b.copy(from = f)()
    }
  }

  /**
   * Since Nodes cannot be encoded into Scala Tuples, they have to be encoded
   * into the Tuples' children instead, so we end up with trees like
   * ProductNode(Wrapped(p, x), Wrapped(p, y)). This optimizer rewrites those
   * forms to Wrapped(p, ProductNode(x, y)).
   */
  def reverseProjectionWrapping = new Transformer {
    def allWrapped(in: Node, xs: Seq[Node]) = xs.forall(_ match {
      case Wrapped(in2, _) if in == in2 => true
      case _ => false
    })
    def replace = {
      case p @ ProductNode(Wrapped(in, _), xs @ _*) if allWrapped(in, xs) =>
        val unwrapped = p.nodeChildren.collect { case Wrapped(_, what) => what }
        Wrapped(in, ProductNode(unwrapped))
    }
  }

  /**
   * Remove unnecessary wrappings of generators
   */
  def unwrapGenerators = new Transformer {
    val defs = new HashMap[Symbol, RefId[Node]]
    val reverse = new HashMap[RefId[Node], Symbol]
    override def initTree(tree: Node) {
      defs.clear()
      defs ++= tree.collectAll[(Symbol, RefId[Node])] {
        case d: DefNode => d.nodeGenerators.map { case (s,n) => (s, RefId(n)) }
      }
      reverse.clear()
      reverse ++= defs.map { case (k,v) => (v,k) }
    }
    def replace = {
      case InRef(sym, Wrapped(in, what)) if {
        (defs.get(sym) == Some(RefId(in))) ||
          defs.get(sym).map(_.e match {
            case Bind(_, _, select) if select eq in => true
            case _ => false
          }).getOrElse(false)
      } => InRef(sym, what)
      case r @ InRef(sym, what) if defs.get(sym) == Some(RefId(what)) => Ref(sym)
      case Wrapped(in, what) if reverse.get(RefId(in)).isDefined => InRef(reverse.get(RefId(in)).get, what)
      case Wrapped(Ref(sym), what) => InRef(sym, what)
    }
  }
}
