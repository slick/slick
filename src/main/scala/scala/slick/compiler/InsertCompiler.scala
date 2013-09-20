package scala.slick.compiler

import scala.slick.ast._
import scala.collection.mutable.ArrayBuffer
import scala.slick.SlickException
import scala.slick.util.SlickLogger
import org.slf4j.LoggerFactory
import Util._

/** A custom compiler for INSERT statements. We could reuse the standard
  * phases with a minor modification instead, but this is much faster. */
trait InsertCompiler extends Phase {
  val name = "insertCompiler"

  override protected[this] lazy val logger = new SlickLogger(LoggerFactory.getLogger(classOf[CodeGen]))

  def apply(state: CompilerState) = state.map { tree =>
    val gen, rgen = new AnonSymbol
    val tref = Ref(gen)
    val rref = Ref(rgen)

    var tableExpansion: TableExpansion = null
    var expansionRef: Symbol = null
    val cols = new ArrayBuffer[Select]
    def setTable(te: TableExpansion) {
      if(tableExpansion eq null) {
        tableExpansion = te
        expansionRef = te.generator
      }
      else if(tableExpansion.table ne te.table) throw new SlickException("Cannot insert into more than one table at once")
    }

    def tr(n: Node): Node = n match {
      case _: OptionApply | _: GetOrElse | _: ProductNode | _: TypeMapping => n.nodeMapChildren(tr, keepType = true)
      case te @ TableExpansion(_, _, expansion) =>
        setTable(te)
        tr(expansion)
      case sel @ Select(Ref(s), fs: FieldSymbol) if s == expansionRef =>
        cols += Select(tref, fs).nodeTyped(sel.nodeType)
        InsertColumn(Select(rref, ElementSymbol(cols.size)).nodeTyped(sel.nodeType), fs).nodeTyped(sel.nodeType)
      case Ref(s) if s == expansionRef =>
        tr(tableExpansion.columns)
      case Bind(gen, te @ TableExpansion(_, t: TableNode, _), Pure(sel, _)) =>
        setTable(te)
        tr(sel.replace({ case Ref(s) if s == gen => Ref(expansionRef) }, keepType = true))
      case _ => throw new SlickException("Cannot use node "+n+" for inserting data")
    }
    val tree2 = tr(tree)
    if(tableExpansion eq null) throw new SlickException("No table to insert into")
    val ins = Insert(gen, tableExpansion.table, tree2, ProductNode(cols)).nodeWithComputedType(SymbolScope.empty, typeChildren = false, retype = true)
    logger.debug("Insert node:", ins)

    ResultSetMapping(rgen, ins, createMapping(ins)).nodeTyped(
      CollectionType(CollectionTypeConstructor.seq, ins.nodeType))
  }

  def createMapping(ins: Insert): Node
}
