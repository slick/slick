package slick.compiler

import slick.ast._
import slick.ast.TypeUtil._
import slick.ast.Util._
import slick.SlickException
import slick.util.{ConstArray, DumpInfo, GlobalConfig, TreePrinter}

/**
  * Resolve an Update node to break the value node into a ProductNode and check its types.
  * Also prepares the Update to be ready to create a ResultSetMapping
  */
class ResolveUpdate extends Phase {
  override val name = "resolveUpdate"

  private val treePrinter =
    new TreePrinter(prefix = DumpInfo.highlight(if(GlobalConfig.unicodeDump) "\u2503 " else "| "))

  /** Run the phase on an outer-bound Update Node */
  override def apply(state: CompilerState): CompilerState = state.map {
    case Bind(_, u: Update, _) => resolveUpdate(u).infer()
    case otherNode => otherNode
  }

  def resolveUpdate(u: Update): Node = {
    val setType = u.set.nodeType.asCollectionType.elementType
    val valueType = u.value.nodeType.asCollectionType.elementType

//    if (!matchRecursive(setType, valueType)) throw new SlickException("Update set and value types do not match!")

    val hasSubSelects = u.value.findNode {
      case Select(_ :@ NominalType(t: TableIdentitySymbol, _), f) => true
      case _ => false
    }.isDefined

    val gen = new AnonSymbol
    val setSyms = u.set.nodeType.structural match {
      case StructType(defs) => defs.map(_._1)
      case CollectionType(_, Type.Structural(StructType(defs))) => defs.map(_._1)
      case t => throw new SlickException("No StructType found in Update.set: "+t)
    }

    val createBind: TermSymbol => Bind = createSubqueryBind(u.value)
    val values = if (!hasSubSelects) u.value match {
      case n: LiteralNode => ConstArray(n)
      case Pure(StructNode(elems), _) => elems.map(_._2)
      case n :@ t => throw new SlickException("Unexpected node found in Update.value: " + n + ": " + t)
    } else u.value.nodeType.structural match {
      case StructType(defs) => defs.map(e => createBind(e._1))
      case CollectionType(_, Type.Structural(StructType(defs))) => defs.map(e => createBind(e._1))
      case t => throw new SlickException("Unexpected nodetype found in Update.value: " + t)
    }

    val appendToPath: (Node, TermSymbol) => Node = (n, s) => n.replace{
      case Path(syms) => Path(syms map { s =>
        Some(s).filter(_ == u.gen).map(_ => gen).getOrElse(s)
      })
    }
    val updates = setSyms.zip(values) map {
      case (sym, node) => {
        val g = new AnonSymbol
        Update(g, Path(sym :: gen :: Nil), appendToPath(node, g))
      }
    }

    UpdateBind(gen, u.set, StructNode(updates map {(new AnonSymbol, _)}), hasSubSelects)
  }

  def createSubqueryBind(n: Node)(sym: TermSymbol): Bind = {
    val gen = new AnonSymbol
    n match {
      case p: Pure => Bind(gen, Pure(ProductNode(ConstArray.empty)), p)
      case _ => Bind(gen, n, Pure(Path(sym :: gen :: Nil)))
    }
  }

  def matchRecursive(t1: Type, t2: Type): Boolean = (t1, t2) match {
    case (at1: AtomicType, at2: AtomicType) => at1 == at2
    case (at1: AtomicType, _) => false
    case (_, at2: AtomicType) => false
    case (ct1, ct2) => (ct1.children zip ct2.children).force.forall { case (t1, t2) => matchRecursive(t1, t2) }
  }
}
