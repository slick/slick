package slick.compiler

import slick.ast._
import slick.{SlickTreeException, SlickException}
import slick.util.{ConstArray, SlickLogger}
import org.slf4j.LoggerFactory
import Util._

/** A custom compiler for INSERT statements. We could reuse the standard
  * phases with a minor modification instead, but this is much faster. */
class InsertCompiler(val mode: InsertCompiler.Mode) extends Phase {
  val name = "insertCompiler"

  override protected[this] lazy val logger = new SlickLogger(LoggerFactory.getLogger(classOf[CodeGen]))

  def apply(state: CompilerState) = state.map { tree =>
    val tableSym, linearSym = new AnonSymbol
    val tref = Ref(tableSym)
    val rref = Ref(linearSym)

    var tableExpansion: TableExpansion = null
    var expansionRef: TermSymbol = null
    val cols = ConstArray.newBuilder[Select]()
    val allFields = ConstArray.newBuilder[FieldSymbol]()
    def setTable(te: TableExpansion): Unit = {
      if(tableExpansion eq null) {
        tableExpansion = te
        expansionRef = te.generator
      }
      else if(tableExpansion.table ne te.table)
        throw new SlickTreeException("Cannot insert into more than one table at once", tree, removeUnmarked = false,
          mark = (n => (n eq tableExpansion.table) || (n eq te.table)))
    }

    def tr(n: Node): Node = n match {
      case _: OptionApply | _: GetOrElse | _: ProductNode | _: TypeMapping => n.mapChildren(tr, keepType = true)
      case OptionFold(from, _, Ref(s2), s1) if s1 == s2 => tr(GetOrElse(from, null).infer())
      case te @ TableExpansion(_, _, expansion) =>
        setTable(te)
        tr(expansion)
      case sel @ Select(Ref(s), fs: FieldSymbol) if s == expansionRef =>
        allFields += fs
        val ch =
          if(mode(fs)) {
            cols += Select(tref, fs) :@ sel.nodeType
            ConstArray(Select(rref, ElementSymbol(cols.length)) :@ sel.nodeType)
          } else ConstArray.empty
        InsertColumn(ch, fs, sel.nodeType).infer()
      case Ref(s) if s == expansionRef =>
        tr(tableExpansion.columns)
      case Bind(gen, te @ TableExpansion(_, t: TableNode, _), Pure(sel, _)) =>
        setTable(te)
        tr(sel.replace({ case Ref(s) if s == gen => Ref(expansionRef) }, keepType = true))
      case _ => throw new SlickException("Cannot use node "+n+" for inserting data")
    }
    val tree2 = tr(tree).infer()
    if(tableExpansion eq null) throw new SlickException("No table to insert into")
    val ins = Insert(tableSym, tableExpansion.table, ProductNode(cols.result), allFields.result).infer()
    ResultSetMapping(linearSym, ins, tree2) :@ CollectionType(TypedCollectionTypeConstructor.seq, ins.nodeType)
  }
}

object InsertCompiler {
  /** Determines which columns to include in the `Insert` and mapping nodes
    * created by `InsertCompiler`. */
  trait Mode extends (FieldSymbol => Boolean)

  /** Include all columns. For use in forced inserts and merges. */
  case object AllColumns extends Mode {
    def apply(fs: FieldSymbol) = true
  }
  /** Include only non-AutoInc columns. For use in standard (soft) inserts. */
  case object NonAutoInc extends Mode {
    def apply(fs: FieldSymbol) = !fs.options.contains(ColumnOption.AutoInc)
  }
  /** Include only primary keys. For use in the insertOrUpdate emulation. */
  case object PrimaryKeys extends Mode {
    def apply(fs: FieldSymbol) = fs.options.contains(ColumnOption.PrimaryKey)
  }
}
