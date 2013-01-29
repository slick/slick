package scala.slick.compiler

import slick.ast.{ClientSideOp, CompiledStatement, ResultSetMapping, Node, First}
import slick.util.SlickLogger
import org.slf4j.LoggerFactory

/** The code generator phase. The actual implementation is provided
  * by a driver. */
abstract class CodeGen extends Phase {
  val name = "codeGen"

  override protected[this] lazy val logger = new SlickLogger(LoggerFactory.getLogger(classOf[CodeGen]))
}

object CodeGen {
  def apply(f: () => ((Node, CompilerState) => (String, Any))): CodeGen = new CodeGen {
    def apply(state: CompilerState): CompilerState = state.map(n => apply(n, state))
    def apply(node: Node, state: CompilerState): Node = node match {
      case c: ClientSideOp =>
        ClientSideOp.mapServerSide(c)(ch => apply(ch, state))
      case n =>
        val (st, ex) = buildStatement(n, state)
        CompiledStatement(st, ex, n.nodeType)
    }
    def buildStatement(n: Node, state: CompilerState): (String, Any) = f().apply(n, state)
  }

  def findResult(n: Node): (String, Any) = n match {
    case r @ ResultSetMapping(_, from, _) => findResult(from)
    case f @ First(from) => findResult(from)
    case CompiledStatement(st, ex, _) => (st, ex)
  }
}
