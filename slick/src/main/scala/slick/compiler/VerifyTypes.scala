package slick.compiler

import slick.SlickTreeException
import slick.ast.Util._
import slick.ast._
import slick.util.{Dumpable, RefId}

import scala.collection.mutable

/** Optional phase which verifies that retyping the tree does not change any types. Useful for
  * debugging type-related problems with large trees. */
class VerifyTypes(onlyServerSide: Boolean = false) extends Phase {
  val name = "verifyTypes"

  def apply(state: CompilerState) = state.map { tree =>
    logger.debug(s"Verifying types (onlyServerSide = $onlyServerSide)")
    if(onlyServerSide) ClientSideOp.mapServerSide(tree)(check)
    else check(tree)
  }

  def check(tree: Node): Node = {
    val retyped = tree.replace({
      case t: TableNode =>
        t.nodeType match {
          case CollectionType(cons, NominalType(ts, _)) =>
          case _ =>
            logger.warn("Table has unexpected type:", t)
        }
        t
      case n => n.untyped
    }, bottomUp = true).infer()

    val errors = mutable.Set.empty[RefId[Dumpable]]

    def compare(n1: Node, n2: Node): Unit = {
      if(n1.nodeType != n2.nodeType) {
        logger.warn("Wrong original type: " + n1.nodeType)
        logger.warn("          should be: " + n2.nodeType)
        errors += RefId(n1)
      }
      (n1.children, n2.children).zipped.map(compare)
    }
    compare(tree, retyped)

    if(errors.nonEmpty)
      throw new SlickTreeException(errors.size+" type errors found:", tree, removeUnmarked = false, mark = (errors contains RefId(_)))

    tree
  }
}
