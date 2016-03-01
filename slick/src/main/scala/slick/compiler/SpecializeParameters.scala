package slick.compiler

import slick.SlickException
import slick.ast._
import Util._
import slick.util.ConstArray

/** Specialize the AST for edge cases of query parameters. This is required for
  * compiling `take(0)` for some databases which do not allow `LIMIT 0`. */
class SpecializeParameters extends Phase {
  val name = "specializeParameters"

  def apply(state: CompilerState): CompilerState =
    state.map(ClientSideOp.mapServerSide(_, keepType = true)(transformServerSide))

  def transformServerSide(n: Node): Node = {
    val cs = n.collect { case c @ Comprehension(_, _, _, _, _, _, _, _, Some(_: QueryParameter), _, _) => c }
    logger.debug("Affected fetch clauses in: "+cs.mkString(", "))
    cs.foldLeft(n) { case (n, c @ Comprehension(_, _, _, _, _, _, _, _, Some(fetch: QueryParameter), _, _)) =>
      val compiledFetchParam = QueryParameter(fetch.extractor, ScalaBaseType.longType)
      val guarded = n.replace({ case c2: Comprehension if c2 == c => c2.copy(fetch = Some(LiteralNode(0L))) }, keepType = true)
      val fallback = n.replace({ case c2: Comprehension if c2 == c => c2.copy(fetch = Some(compiledFetchParam)) }, keepType = true)
      ParameterSwitch(ConstArray(compare(fetch.extractor, 0L) -> guarded), fallback).infer()
    }
  }

  /** Create a function that calls an extractor for a value and compares the result with a fixed value. */
  def compare(f: (Any => Any), v: Any) = new (Any => Boolean) {
    def apply(param: Any) = v == f(param)
    override def toString = s"$f(...) == $v"
  }
}
