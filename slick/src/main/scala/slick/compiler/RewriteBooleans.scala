package slick.compiler

import slick.ast._
import Util.nodeToNodeOps
import TypeUtil._

/** For SQL back-ends which do not support real boolean types for fields and general expressions
  * but which do have special boolean expressions and operators, this phase injects conversions
  * between fake and real boolean values.
  *
  * The default for booleans in the AST is to use the fake type (mapped to a numeric type by the
  * profile). There are specific places where a real boolean (that can be used in boolean
  * expressions) is required or produced, so we inject a call to ToRealBoolean or ToFakeBoolean as
  * needed. */
class RewriteBooleans extends Phase {
  import RewriteBooleans._
  val name = "rewriteBooleans"

  def apply(state: CompilerState) =
    state.map { n => ClientSideOp.mapServerSide(n)(rewriteRec) }

  def rewriteRec(n: Node): Node = {
    val n2 = n.mapChildren(rewriteRec, true)
    val n3 = rewrite(n2)
    if(n3 ne n2) logger.debug(s"Rewriting $n2 to $n3")
    n3
  }

  /** Rewrite a single Node. This method can be overridden in subclasses to
    * change the situations in which conversions are applied. */
  def rewrite(n: Node): Node = n match {
    // These boolean operators accept and produce real booleans
    case Apply(sym @ (Library.And | Library.Or | Library.Not), ch) =>
      toFake(Apply(sym, ch.map(n => toReal(n)))(n.nodeType).infer())
    // All other boolean-typed operators produce real booleans but accept fake ones
    case Apply(sym, ch) :@ tpe if isBooleanLike(tpe) =>
      toFake(Apply(sym, ch)(n.nodeType).infer())
    // Where clauses, join conditions and case clauses need real boolean predicates
    case n @ Comprehension(_, _, _, where, _, _, having, _, _, _, _) =>
      n.copy(where = where.map(toReal), having = having.map(toReal)) :@ n.nodeType
    case n @ Join(_, _, _, _, _, on) =>
      n.copy(on = toReal(on)) :@ n.nodeType
    case cond @ IfThenElse(_) =>
      cond.mapConditionClauses(toReal) :@ cond.nodeType
    case n => n
  }

  /** Create a conversion to a fake boolean, cancelling out an existing
    * conversion to a real boolean. */
  def toFake(n: Node) = n match {
    case ToRealBoolean(ch) => ch
    case _ => ToFakeBoolean.typed(n.nodeType, n).infer()
  }

  /** Create a conversion to a real boolean, cancelling out an existing
    * conversion to a fake boolean. */
  def toReal(n: Node) = n match {
    case ToFakeBoolean(ch) => ch
    case _ => ToRealBoolean.typed(n.nodeType, n).infer()
  }

  /** Check if a type is equivalent to the Scala Boolean type or a (possibly
    * nested) Option of that type. */
  def isBooleanLike(t: Type): Boolean = t match {
    case t: TypedType[_] if t.scalaType == ScalaBaseType.booleanType => true
    case t: OptionType => isBooleanLike(t.elementType)
    case _ => false
  }
}

object RewriteBooleans {
  val ToFakeBoolean = new FunctionSymbol("RewriteBooleans.ToFakeBoolean")
  val ToRealBoolean = new FunctionSymbol("RewriteBooleans.ToRealBoolean")
}
