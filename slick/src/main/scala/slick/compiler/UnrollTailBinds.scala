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
      Bind(bo, Filter(fa, ff1, where1), Filter(fb, ff2, where2)),
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
          // bind all needed elements to the new symbol
          val bindCombo = Bind(bo,
            Filter(fa, rep(ff1), rep(where1)), //replace s4 here with s5
            Bind(bm,
              Filter(fb, rep(ff2), rep(where2)), // replace s4 here with s5
              Bind(bi, rep(bf), rep(select)))) // replace s4 here with s5

          bindCombo.mapChildren(tr)
        }
      case n => n.mapChildren(tr)

    }
  }

}
