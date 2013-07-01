package scala.slick.compiler

import scala.slick.ast._
import scala.collection.mutable.ArrayBuffer
import scala.slick.SlickException
import scala.slick.util.SlickLogger
import org.slf4j.LoggerFactory

/** A custom compiler for INSERT statements. We could reuse the standard
  * phases with a minor modification instead, but this is much faster. */
trait InsertCompiler extends Phase {
  val name = "insertCompiler"

  override protected[this] lazy val logger = new SlickLogger(LoggerFactory.getLogger(classOf[CodeGen]))

  def apply(state: CompilerState) = state.map { tree =>
    val gen, rgen = new AnonSymbol
    val tref = Ref(gen)
    val rref = Ref(rgen)

    var table: TableNode = null
    val cols = new ArrayBuffer[Select]

    def tr(n: Node): Node = n match {
      case _: OptionApply | _: GetOrElse | _: ProductNode | _: TypeMapping => n.nodeMapChildren(tr, keepType = true)
      case t:TableNode => tr(Node(t.nodeTableProjection))
      case sel @ Select(Ref(IntrinsicSymbol(t: TableNode)), fs: FieldSymbol) =>
        if(table eq null) table = t
        else if(table ne t) throw new SlickException("Cannot insert into more than one table at once")
        cols += Select(tref, fs).nodeTyped(sel.nodeType)
        Select(rref, ElementSymbol(cols.size)).nodeTyped(sel.nodeType)
      case _ => throw new SlickException("Cannot use node "+n+" for inserting data")
    }
    val tree2 = tr(tree)
    if(table eq null) throw new SlickException("No table to insert into")
    val ins = Insert(gen, table, tree2, ProductNode(cols)).nodeWithComputedType(SymbolScope.empty, typeChildren = false, retype = true)
    logger.debug("Insert node:", ins)

    ResultSetMapping(rgen, ins, createMapping(ins)).nodeTyped(
      CollectionType(CollectionTypeConstructor.default, ins.nodeType))
  }

  def createMapping(ins: Insert): Node
}
