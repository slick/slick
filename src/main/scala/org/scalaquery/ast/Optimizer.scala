package org.scalaquery.ast

import collection.mutable.{ArrayBuffer, HashMap}
import org.scalaquery.ql.{Pure, Bind, FilteredQuery}
import OptimizerUtil._

/**
 * Basic optimizers for the ScalaQuery AST
 */
object Optimizer {

  /**
   * Eliminate unnecessary nodes from the AST.
   */
  def eliminateIndirections = new Transformer {
    def replace = {
      // Remove alias if aliased node or alias is not referenced otherwise
      case a @ Alias(ch) if unique(ch) || unique(a) => ch
      // Remove wrapping of the entire result of a FilteredQuery
      case Wrapped(q @ FilteredQuery(from), what) if what == from => q
      // Remove dual wrapping (remnant of a removed Filter(_, ConstColumn(true)))
      case Wrapped(in2, w2 @ Wrapped(in1, what)) if in1 == in2 => w2
      // Remove identity binds
      case Bind(x, Pure(y)) if x == y => x
      // Remove unnecessary wrapping of pure values
      case Wrapped(p @ Pure(n1), n2) if n1 == n2 => p
      // Remove unnecessary wrapping of binds
      case Wrapped(b @ Bind(_, s1), s2) if s1 == s2 => b
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
}
