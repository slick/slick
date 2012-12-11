package scala.slick.compiler

import slick.ast.{CompiledStatement, ResultSetMapping, Node}
import org.slf4j.LoggerFactory
import slick.util.SlickLogger

/** The code generator phase. The actual implementation is provided
  * by a driver. */
abstract class CodeGen[Extra] extends Phase {
  val name = "codeGen"

  override protected[this] lazy val logger = new SlickLogger(LoggerFactory.getLogger(classOf[CodeGen[_]]))

  def apply(tree: Node, state: CompilationState): Node = tree match {
    case r @ ResultSetMapping(_, from, _) =>
      r.copy(from = apply(from, state))
    case n =>
      val (st, ex) = buildStatement(n, state)
      CompiledStatement(st, ex, n.nodeType)
  }

  def buildStatement(n: Node, state: CompilationState): (String, Extra)
}

object CodeGen {
  def apply[Extra](f: () => ((Node, CompilationState) => (String, Extra))): CodeGen[Extra] = new CodeGen[Extra] {
    def buildStatement(n: Node, state: CompilationState): (String, Extra) =
      f().apply(n, state)
  }

  def findResult(n: Node): (String, Any) = n match {
    case r @ ResultSetMapping(_, from, _) => findResult(from)
    case CompiledStatement(st, ex, _) => (st, ex)
  }
}
