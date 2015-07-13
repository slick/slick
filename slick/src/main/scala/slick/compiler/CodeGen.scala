package slick.compiler

import slick.ast.{ClientSideOp, CompiledStatement, ResultSetMapping, Node, First}
import slick.util.SlickLogger

/** A standard skeleton for a code generator phase. */
abstract class CodeGen extends Phase {
  val name = "codeGen"

  override protected[this] lazy val logger = SlickLogger[CodeGen]

  def apply(state: CompilerState): CompilerState = state.map(n => apply(n, state))

  def apply(node: Node, state: CompilerState): Node =
    ClientSideOp.mapResultSetMapping(node, keepType = true) { rsm =>
      var nmap: Option[Node] = None
      var compileMap: Option[Node] = Some(rsm.map)

      val nfrom = ClientSideOp.mapServerSide(rsm.from, keepType = true) { ss =>
        logger.debug("Compiling server-side and mapping with server-side:", ss)
        val (nss, nmapOpt) = compileServerSideAndMapping(ss, compileMap, state)
        nmapOpt match {
          case Some(_) =>
            nmap = nmapOpt
            compileMap = None
          case None =>
        }
        logger.debug("Compiled server-side to:", nss)
        nss
      }
      rsm.copy(from = nfrom, map = nmap.get) :@ rsm.nodeType
    }

  def compileServerSideAndMapping(serverSide: Node, mapping: Option[Node], state: CompilerState): (Node, Option[Node])
}

object CodeGen {
  def findResult(n: Node): (String, Any) = n match {
    case r @ ResultSetMapping(_, from, _) => findResult(from)
    case f @ First(from) => findResult(from)
    case CompiledStatement(st, ex, _) => (st, ex)
  }
}
