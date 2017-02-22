package slick.compiler

import slick.ast._
import slick.ast.Util._
import slick.ast.TypeUtil._
import slick.util.{ConstArray, Ellipsis}

/** Reorder certain stream operations for more efficient merging in `mergeToComprehensions`. */
class ReorderOperations extends Phase {
  val name = "reorderOperations"

  def apply(state: CompilerState) = state.map(convert)

  def convert(tree: Node): Node = tree.replace({ case n => convert1(n) }, keepType = true, bottomUp = true)

  def convert1(tree: Node): Node = tree match {
    // Push Bind into Union
    case n @ Bind(s1, Union(l1, r1, all), sel) =>
      logger.debug("Pushing Bind into both sides of a Union", Ellipsis(n, List(0, 0), List(0, 1)))
      val s1l, s1r = new AnonSymbol
      val n2 = Union(
        Bind(s1l, l1, sel.replace { case Ref(s) if s == s1 => Ref(s1l) }),
        Bind(s1r, r1, sel.replace { case Ref(s) if s == s1 => Ref(s1r) }),
        all).infer()
      logger.debug("Pushed Bind into both sides of a Union", Ellipsis(n2, List(0, 0), List(1, 0)))
      n2

    // Push Filter into Union
    case n @ Filter(s1, Union(l1, r1, all), pred) =>
      logger.debug("Pushing Filter into both sides of a Union", Ellipsis(n, List(0, 0), List(0, 1)))
      val s1l, s1r = new AnonSymbol
      val n2 = Union(
        Filter(s1l, l1, pred.replace { case Ref(s) if s == s1 => Ref(s1l) }),
        Filter(s1r, r1, pred.replace { case Ref(s) if s == s1 => Ref(s1r) }),
        all).infer()
      logger.debug("Pushed Filter into both sides of a Union", Ellipsis(n2, List(0, 0), List(1, 0)))
      n2

    // Push CollectionCast into Union
    case n @ CollectionCast(Union(l1, r1, all), cons) =>
      logger.debug("Pushing CollectionCast into both sides of a Union", Ellipsis(n, List(0, 0), List(0, 1)))
      val n2 = Union(CollectionCast(l1, cons), CollectionCast(r1, cons), all).infer()
      logger.debug("Pushed CollectionCast into both sides of a Union", Ellipsis(n2, List(0, 0), List(1, 0)))
      n2

    // Remove Subquery boundary on top of TableNode and Join
    case Subquery(n @ (_: TableNode | _: Join), _) => n

    // Push distinctness-preserving aliasing / literal projection into Subquery.AboveDistinct
    case n @ Bind(s, Subquery(from :@ CollectionType(_, tpe), Subquery.AboveDistinct), Pure(StructNode(defs), ts1))
        if isAliasingOrLiteral(s, defs) && isDistinctnessPreserving(s, defs, tpe) =>
      Subquery(n.copy(from = from), Subquery.AboveDistinct).infer()

    // Push Take and Drop (always distinctness-preserving) into Subquery.AboveDistinct
    case Take(Subquery(from, Subquery.AboveDistinct), count) =>
      Subquery(Take(from, count), Subquery.AboveDistinct).infer()
    case Drop(Subquery(from, Subquery.AboveDistinct), count) =>
      Subquery(Drop(from, count), Subquery.AboveDistinct).infer()

    // Push any aliasing / literal projection into other Subquery
    case n @ Bind(s, Subquery(from, cond), Pure(StructNode(defs), ts1)) if cond != Subquery.AboveDistinct && cond != Subquery.BelowRowNumber && isAliasingOrLiteral(s, defs) =>
      Subquery(n.copy(from = from), cond).infer()

    // If a Filter checks an upper bound of a ROWNUM, push it into the AboveRownum boundary
    case filter @ Filter(s1,
                sq @ Subquery(bind @ Bind(bs1, from1, Pure(StructNode(defs1), ts1)), Subquery.AboveRownum),
                Apply(Library.<= | Library.<, ConstArray(Select(Ref(rs), f1), v1)))
        if rs == s1 && defs1.find {
          case (f, n) if f == f1 => isRownumCalculation(n)
          case _ => false
        }.isDefined =>
      sq.copy(child = filter.copy(from = bind)).infer()

    // Push a BelowRowNumber boundary into SortBy
    case sq @ Subquery(n: SortBy, Subquery.BelowRowNumber) =>
      n.copy(from = convert1(sq.copy(child = n.from))).infer()

    // Push a BelowRowNumber boundary into Filter
    case sq @ Subquery(n: Filter, Subquery.BelowRowNumber) =>
      n.copy(from = convert1(sq.copy(child = n.from))).infer()

    // Push a BelowRowNumber boundary into aliasing / literal projection
    case sq @ Subquery(n @ Bind(s, from, Pure(StructNode(defs), ts1)), Subquery.BelowRowNumber) if isAliasingOrLiteral(s, defs) =>
      n.copy(from = convert1(sq.copy(child = from))).infer()

    case n => n
  }

  def isAliasingOrLiteral(base: TermSymbol, defs: ConstArray[(TermSymbol, Node)]) = {
    val r = defs.iterator.map(_._2).forall {
      case FwdPath(s :: _) if s == base => true
      case _: LiteralNode => true
      case _: QueryParameter => true
      case _ => false
    }
    logger.debug("Bind from "+base+" is aliasing / literal: "+r)
    r
  }

  def isDistinctnessPreserving(base: TermSymbol, defs: ConstArray[(TermSymbol, Node)], tpe: Type) = {
    val usedFields = defs.flatMap(_._2.collect[TermSymbol] {
      case Select(Ref(s), f) if s == base => f
    })
    val StructType(tDefs) = tpe.structural
    (tDefs.map(_._1).toSet -- usedFields.toSeq).isEmpty
  }

  def isRownumCalculation(n: Node): Boolean = n match {
    case Apply(Library.+ | Library.-, ch) => ch.exists(isRownumCalculation)
    case _: RowNumber => true
    case _ => false
  }
}
