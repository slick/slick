package org.scalaquery.ast

import OptimizerUtil._
import org.scalaquery.util.RefId
import org.scalaquery.ql.RawNamedColumn
import scala.collection.mutable.{ArrayBuffer, HashMap}

/**
 * Basic optimizers for the ScalaQuery AST
 */
object Optimizer {

  lazy val all =
    eliminateIndirections andThen
    unwrapGenerators andThen
    reverseProjectionWrapping

  /**
   * Eliminate unnecessary nodes from the AST.
   */
  val eliminateIndirections = new Transformer {
    def replace = {
      // Remove wrapping of the entire result of a FilteredQuery
      case Wrapped(q @ FilteredQuery(_, from), what) if what == from => q
      // Remove dual wrapping (remnant of a removed Filter(_, ConstColumn(true)))
      case Wrapped(in2, w2 @ Wrapped(in1, what)) if in1 == in2 => w2
      // Remove identity binds
      case Bind(_, x, Pure(y)) if x == y => x
      // Remove unnecessary wrapping of pure values
      case Wrapped(p @ Pure(n1), n2) if n1 == n2 => p
      // Remove unnecessary wrapping of binds
      case Wrapped(b @ Bind(_, _, s1), s2) if s1 == s2 => b
      // Remove unnecessary wrapping of filters in FROM clauses
      case b @ Bind(gen, Wrapped(f: FilteredQuery, _), what) => b.copy(from = f)
      case b @ Bind(gen, Wrapped(f: FilteredJoin, _), what) => b.copy(from = f)
      // Unwrap unions
      case Wrapped(u @ Union(left, _, _, _, _), what) if left == what => u
    }
  }

  /**
   * Since Nodes cannot be encoded into Scala Tuples, they have to be encoded
   * into the Tuples' children instead, so we end up with trees like
   * ProductNode(Wrapped(p, x), Wrapped(p, y)). This optimizer rewrites those
   * forms to Wrapped(p, ProductNode(x, y)).
   */
  val reverseProjectionWrapping = new Transformer {
    def allWrapped(in: Node, xs: Seq[Node]) = xs.forall(_ match {
      case Wrapped(in2, _) if in == in2 => true
      case _ => false
    })
    def replace = {
      case p @ ProductNode(Wrapped(in, _), xs @ _*) if allWrapped(in, xs) =>
        val unwrapped = p.nodeChildren.collect { case Wrapped(_, what) => what }
        Wrapped(in, ProductNode(unwrapped: _*))
    }
  }

  /**
   * Remove unnecessary wrappings of generators
   */
  val unwrapGenerators = new Transformer.DefRefsBidirectional {
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
      case Wrapped(InRefChain(syms, Ref(sym2)), what) => InRefChain(syms :+ sym2, what)
      //case Wrapped(u @ Union(sym1, _, _, _, _), Ref(sym2)) if sym1 == sym2 => u
      //case Wrapped(f @ FilteredQuery(gen1, from), InRef(gen2, what)) if gen1 == gen2 && from == what => f
      case InRef(sym, what) if (defs.get(sym) match {
        case Some(RefId(FilteredQuery(_, from))) if what == from => true
        case _ => false
      }) => defs(sym).e
      //-- case InRef(sym, RawNamedColumn(name, _)) => Path(sym, FieldSymbol(name))
      /*case i @ InRef(sym1, Path(sym2, rest @ _*)) =>
        defs.get(sym1) match {
          case Some(RefId(FilteredJoin(g1, g2, _, _, _, _))) if sym2 == g1 || sym2 == g2 =>
            Path((sym1 +: sym2 +: rest): _*)
          case Some(RefId(RealFilterChain(defs, _))) =>
            Path((sym1 +: (sym2 +: rest).filterNot(defs contains _) ): _*)
          case _ => i
        }*/
    }
  }

  /**
   * An extractor for the transitive source of a chain of FilteredQuery nodes
   */
  object FilterChain {
    def unapply(n: Node): Some[(List[Symbol], Node)] = n match {
      case FilteredQuery(sym, from) =>
        val (ss, n) = unapply(from).get
        Some((sym :: ss, n))
      case n => Some(Nil, n)
    }
  }

  /**
   * An extractor for the transitive source of a chain of FilteredQuery nodes
   * which must actually start with a FilteredQuery.
   */
  object RealFilterChain {
    def unapply(n: Node): Option[(List[Symbol], Node)] = n match {
      case _: FilteredQuery => FilterChain.unapply(n)
      case _ => None
    }
  }

  /**
   * A constructor and  extractor for a chain of InRef nodes
   */
  object InRefChain {
    def apply(syms: Seq[Symbol], what: Node) =
      syms.foldRight(what){ case (sym,z) => InRef(sym, z) }
    def unapply(n: Node): Option[(List[Symbol], Node)] = n match {
      case InRef(sym, what) =>
        unapply(what).map{ case (ss, n) => (sym :: ss, n) }.orElse(Some((List(sym), what)))
      case n => None
    }
  }

  /**
   * An extractor for nested ProductNodes (does not match non-nested ones)
   */
  object NestedProductNode {
    def unapplySeq(p: ProductNode): Option[Seq[Node]] = {
      var nested = false
      val b = new ArrayBuffer[Node]
      def scan(n: Node): Unit = n match {
        case ProductNode(ch @ _*) =>
          nested = true
          ch.foreach(scan)
        case n => b += n
      }
      p.nodeChildren.foreach(scan)
      if(nested) Some(b)
      else None
    }
  }
}
