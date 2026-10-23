package slick.compiler

import slick.ast._

/** For Boolean types that the database does not store as booleans, such as a Boolean mapped to a
  * column holding 'Y'/'N', this phase turns a value used where SQL requires a boolean predicate (a
  * WHERE or HAVING clause, a join condition, a CASE WHEN condition, or an operand of AND, OR or NOT)
  * into a comparison with the type's own encoding of `true`.
  *
  * @param isMappedBoolean whether a type is a Boolean (or an Option of one) that the database does
  *   not store as a boolean */
class RewriteMappedBooleans(isMappedBoolean: Type => Boolean) extends Phase {
  val name = "rewriteMappedBooleans"

  def apply(state: CompilerState) =
    state.map { n => ClientSideOp.mapServerSide(n)(rewriteRec) }

  def rewriteRec(n: Node): Node = rewrite(n.mapChildren(rewriteRec, true))

  def rewrite(n: Node): Node = n match {
    case Apply(sym @ (Library.And | Library.Or | Library.Not), ch) =>
      Apply(sym, ch.map(toPredicate))(n.nodeType).infer()
    case n @ Comprehension(_, _, _, where, _, _, having, _, _, _, _) =>
      n.copy(where = where.map(toPredicate), having = having.map(toPredicate)) :@ n.nodeType
    case n @ Join(_, _, _, _, _, on) =>
      n.copy(on = toPredicate(on)) :@ n.nodeType
    case cond @ IfThenElse(_) =>
      cond.mapConditionClauses(toPredicate) :@ cond.nodeType
    case n => n
  }

  /** Compare a value of a mapped Boolean type with the type's `true`. AND, OR and NOT are already
    * predicates, even when they carry the mapped type of their operands. */
  def toPredicate(n: Node): Node = n match {
    case Apply(Library.And | Library.Or | Library.Not, _) => n
    case _ if isMappedBoolean(n.nodeType) =>
      val tpe = n.nodeType.structural match {
        case OptionType(t) => t
        case t => t
      }
      Library.==.typed[Boolean](n, LiteralNode(tpe, true)).infer()
    case _ => n
  }
}
