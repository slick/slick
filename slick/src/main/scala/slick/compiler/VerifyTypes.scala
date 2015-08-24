package slick.compiler

import slick.SlickTreeException
import slick.ast.Util._
import slick.ast._
import slick.util.{Dumpable, RefId}

import scala.collection.mutable

/** Optional phase which verifies that retyping the tree does not change any types. Useful for
  * debugging type-related problems with large trees. */
class VerifyTypes(after: Option[Phase] = None) extends Phase {
  val name = "verifyTypes"

  def apply(state: CompilerState) = state.map { tree =>
    logger.debug(s"Verifying types")
    check(tree)
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
    var nodeCount = 0

    def compare(n1: Node, n2: Node): Unit = {
      if(n1.nodeType != n2.nodeType) {
        nodeCount += 1
        if(!errors.contains(RefId(n1))) {
          logger.warn("Wrong original type: " + n1.nodeType)
          logger.warn("          should be: " + n2.nodeType)
          errors += RefId(n1)
        }
      }
      n1.children.zip(n2.children).force.foreach { case (n1, n2) => compare(n1, n2) }
    }
    compare(tree, retyped)

    if(errors.nonEmpty)
      throw new SlickTreeException(
        after.map(p => "After "+p.name+": ").getOrElse("")+errors.size+" type errors found in "+nodeCount+" nodes:",
        tree, removeUnmarked = false, mark = (errors contains RefId(_)))

    tree
  }
}
