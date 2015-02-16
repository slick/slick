package slick.compiler

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
      var nmap: Option[Node] = None
      var compileMap: Option[Node] = Some(rsm.map)

      val nfrom = ClientSideOp.mapServerSide(rsm.from, keepType = true) { ss =>
        val (nss, nmapOpt) = compileServerSideAndMapping(ss, compileMap, state)
        nmapOpt match {
          case Some(_) =>
            nmap = nmapOpt
            compileMap = None
          case None =>
        }
        nss
      }
      rsm.copy(from = nfrom, map = nmap.get).nodeTyped(rsm.nodeType)
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
