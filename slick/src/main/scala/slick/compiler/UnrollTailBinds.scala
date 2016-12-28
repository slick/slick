package slick.compiler

import slick.ast.Library.AggregateFunctionSymbol

import scala.collection.mutable.{HashSet, HashMap}
import slick.SlickException
import slick.ast._
import TypeUtil._
import Util._

class UnrollTailBinds extends Phase {
  val name = "unrollTailBinds"

  def apply(state: CompilerState) = state.map(tr(_))

  def tr(n: Node): Node = {
    n match {
      case bb@Bind(br,
      downstream:Bind,
      Bind(bi, bf, select)) => {

        // make a new symbol
        val bm = new AnonSymbol
        def rep(node: Node) = {
          def repInternal(node: Node): Node = {
            val out = node.replace({
              case p@Path(brs :: tail) if brs == br => {
                Path(List(bm) ++ tail)
              }
            }, bottomUp = true, keepType = true)
            out.mapChildren(repInternal)
          }
          repInternal(node)
        }

        def replaceDownstream(node:Bind):Option[Node] = {
          node match {
            case Bind(bo, Filter(fa, ff1, where1), Filter(fb, ff2, where2)) => {
              val out = Bind(bo,
                Filter(fa, rep(ff1), rep(where1)),
                Bind(bm,
                  Filter(fb, rep(ff2), rep(where2)),
                  Bind(bi, rep(bf), rep(select))))
              Some(out)
            }
            case Bind(bo1, bi1:Bind, sel1) => replaceDownstream(bi1).map(
              replacement => Bind(bo1, replacement, sel1))
            case n => None
          }
        }

        // bind all needed elements to the new symbol
        val bindCombo = replaceDownstream(downstream).getOrElse(bb)

        bindCombo.mapChildren(tr)
      }

      case n => n.mapChildren(tr)
    }
  }

}
