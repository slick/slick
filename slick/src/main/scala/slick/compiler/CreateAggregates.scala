package slick.compiler

import slick.ast.Library.AggregateFunctionSymbol
import slick.ast.TypeUtil._
import slick.ast.Util._
import slick.ast._
import slick.util.ConstArray

/** Rewrite aggregation function calls to Aggregate nodes. */
class CreateAggregates extends Phase {
  val name = "createAggregates"

  def apply(state: CompilerState) = {
    if(state.get(Phase.assignUniqueSymbols).map(_.aggregate).getOrElse(true))
      state.map(_.replace({
        case n @ Apply(f: AggregateFunctionSymbol, ConstArray(from)) =>
          logger.debug("Converting aggregation function application", n)
          val CollectionType(_, elType @ Type.Structural(StructType(els))) = from.nodeType
          val s = new AnonSymbol
          val a = Aggregate(s, from, Apply(f, ConstArray(f match {
            case Library.CountAll => LiteralNode(1)
            case _ => Select(Ref(s) :@ elType, els.head._1) :@ els.head._2
          }))(n.nodeType)).infer()
          logger.debug("Converted aggregation function application", a)
          inlineMap(a)

        case n @ Bind(s1, from1, Pure(sel1, ts1)) if !from1.isInstanceOf[GroupBy] =>
          val (sel2, temp) = liftAggregates(sel1, s1)
          if(temp.isEmpty) n else {
            logger.debug("Lifting aggregates into join in:", n)
            logger.debug("New mapping with temporary refs:", sel2)
            val sources = (from1 match {
              case Pure(StructNode(ConstArray()), _) => Vector.empty[(TermSymbol, Node)]
              case _ => Vector(s1 -> from1)
            }) ++ temp.map { case (s, n) => (s, Pure(n)) }
            val from2 = sources.init.foldRight(sources.last._2) {
              case ((_, n), z) => Join(new AnonSymbol, new AnonSymbol, n, z, JoinType.Inner, LiteralNode(true))
            }.infer()
            logger.debug("New 'from' with joined aggregates:", from2)
            val repl: Map[TermSymbol, List[TermSymbol]] = sources match {
              case Vector((s, n)) => Map(s -> List(s1))
              case _ =>
                val len = sources.length
                val it = Iterator.iterate(s1)(_ => ElementSymbol(2))
                sources.zipWithIndex.map { case ((s, _), i) =>
                  val l = List.iterate(s1, i+1)(_ => ElementSymbol(2))
                  s -> (if(i == len-1) l else l :+ ElementSymbol(1))
                }.toMap
            }
            logger.debug("Replacement paths: " + repl)
            val scope = Type.Scope(s1 -> from2.nodeType.asCollectionType.elementType)
            val replNodes = repl.mapValues(ss => FwdPath(ss).infer(scope))
            logger.debug("Replacement path nodes: ", StructNode(ConstArray.from(replNodes)))
            val sel3 = sel2.replace({ case n @ Ref(s) => replNodes.getOrElse(s, n) }, keepType = true)
            val n2 = Bind(s1, from2, Pure(sel3, ts1)).infer()
            logger.debug("Lifted aggregates into join in:", n2)
            n2
          }
      }, keepType = true, bottomUp = true))
    else state
  }

  /** Recursively inline mapping Bind calls under an Aggregate */
  def inlineMap(a: Aggregate): Aggregate = a.from match {
    case Bind(s1, f1, Pure(StructNode(defs1), ts1)) if !f1.isInstanceOf[GroupBy] => // mergeToComprehensions always needs a Bind around a GroupBy
      logger.debug("Inlining mapping Bind under Aggregate", a)
      val defs1M = defs1.iterator.toMap
      val sel = a.select.replace({
        case FwdPath(s :: f :: rest) if s == a.sym =>
          rest.foldLeft(defs1M(f)) { case (n, s) => n.select(s) }.infer()
      }, keepType = true)
      val a2 = Aggregate(s1, f1, sel) :@ a.nodeType
      logger.debug("Inlining mapping Bind under Aggregate", a2)
      inlineMap(a2)
    case _ => a
  }

  /** Find all scalar Aggregate calls in a sub-tree that do not refer to the given Symbol,
    * and replace them by temporary Refs. */
  def liftAggregates(n: Node, outer: TermSymbol): (Node, Map[TermSymbol, Aggregate]) = n match {
    case a @ Aggregate(s1, f1, sel1) =>
      if(a.findNode {
          case n: PathElement => n.sym == outer
          case _ => false
        }.isDefined) (a, Map.empty)
      else {
        val s, f = new AnonSymbol
        val a2 = Aggregate(s1, f1, StructNode(ConstArray(f -> sel1))).infer()
        (Select(Ref(s) :@ a2.nodeType, f).infer(), Map(s -> a2))
      }
    case n :@ CollectionType(_, _) =>
      (n, Map.empty)
    case n =>
      val mapped = n.children.map(liftAggregates(_, outer))
      val m = mapped.iterator.flatMap(_._2).toMap
      val n2 =
        if(m.isEmpty) n else n.withChildren(mapped.map(_._1)) :@ n.nodeType
      (n2, m)
  }
}
