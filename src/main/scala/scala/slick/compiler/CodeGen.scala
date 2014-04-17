package scala.slick.compiler

import slick.ast.{ClientSideOp, CompiledStatement, ResultSetMapping, Node, First}
import slick.util.SlickLogger
import org.slf4j.LoggerFactory

/** A standard skeleton for a code generator phase. */
abstract class CodeGen extends Phase {
  val name = "codeGen"

  override protected[this] lazy val logger = new SlickLogger(LoggerFactory.getLogger(classOf[CodeGen]))

  def apply(state: CompilerState): CompilerState = state.map(n => apply(n, state))

  def apply(node: Node, state: CompilerState): Node =
    ClientSideOp.mapResultSetMapping(node, keepType = true) { rsm =>
      val nfrom = ClientSideOp.mapServerSide(rsm.from, keepType = true)(ss => compileServerSide(ss, state))
      val nmap = compileMapping(rsm.map, state, nfrom)
      rsm.copy(from = nfrom, map = nmap).nodeTyped(rsm.nodeType)
    }

  def compileServerSide(n: Node, state: CompilerState): Node

  def compileMapping(n: Node, state: CompilerState, serverSide: Node): Node
}

object CodeGen {
  def findResult(n: Node): (String, Any) = n match {
    case r @ ResultSetMapping(_, from, _) => findResult(from)
    case f @ First(from) => findResult(from)
    case CompiledStatement(st, ex, _) => (st, ex)
  }
}
