package slick.compiler

import slick.util.ConstArray

import scala.collection.mutable.{ArrayBuffer, HashMap}
import slick.ast._
import TypeUtil._

/** Flatten all `Pure` node contents into a single `StructNode`.
  *
  * After this phase, all `Pure` nodes produce a `StructNode` of primitive fields. As a
  * side-effect, nested NominalTypes are eliminated. */
class FlattenProjections extends Phase {
  val name = "flattenProjections"

  def apply(state: CompilerState) = state.map { tree =>
    val translations = new HashMap[TypeSymbol, Map[List[TermSymbol], TermSymbol]]
    def tr(n: Node, topLevel: Boolean): Node = n match {
      case Pure(v, ts) =>
        logger.debug(s"Flattening projection $ts")
        val (newV, newTranslations) = flattenProjection(tr(v, false), !topLevel)
        translations += ts -> newTranslations
        logger.debug(s"Adding translation for $ts: ($newTranslations, ${newV.nodeType})")
        val res = Pure(newV, ts)
        logger.debug("Flattened projection to", res)
        res
      case p: PathElement =>
        logger.debug("Analyzing "+p.pathString+" with symbols "+translations.keySet.mkString(", "), p)
        val p2 = splitPath(p, translations.keySet) match {
          case Some((base, rest, tsym)) =>
            logger.debug("Found "+p.pathString+" with local part "+Path.toString(rest)+" over "+tsym)
            val paths = translations(tsym)
            logger.debug(s"  Translation for $tsym: $paths")
            Select(base.untypedPath, paths(rest))
          case None => p.untypedPath
        }
        logger.debug("Translated "+p.pathString+" to:", p2)
        p2
      case n: Bind =>
        n.mapChildren { ch => tr(ch, topLevel && (ch ne n.from)) }
      case u: Union =>
        n.mapChildren { ch => tr(ch, true) }
      case _: ClientSideOp =>
        n.mapChildren { ch => tr(ch, topLevel) }
      case Library.SilentCast(ch) :@ tpe =>
        Library.SilentCast.typed(tpe.structuralRec, tr(ch, false))
      case n => n.mapChildren(tr(_, false))
    }
    tr(tree, true).infer()
  }

  /** Split a path into the shortest part with a NominalType and the rest on
    * top of it. Returns `None` if there is no NominalType with one of the
    * candidate TypeSymbols in the path, otherwise returns `Some(base, rest, tsym)`
    * where `tsym` is the symbol that was found and `base` is the Node in the path
    * which has a NominalType of that symbol. `rest` is a (possibly empty) path of
    * symbols on top of `base`. */
  def splitPath(n: PathElement, candidates: scala.collection.Set[TypeSymbol]): Option[(PathElement, List[TermSymbol], TypeSymbol)] = {
    def checkType(tpe: Type): Option[(PathElement, List[TermSymbol], TypeSymbol)] = tpe match {
      case NominalType(tsym, _) if candidates contains tsym => Some((n, Nil, tsym))
      case _ => None
    }
    n match {
      case Select(in, field) => splitPath(in.asInstanceOf[PathElement], candidates) match {
        case Some((n, p, tsym)) => Some((n, field :: p, tsym))
        case None => checkType(n.nodeType)
      }
      case _: Ref => checkType(n.nodeType)
    }
  }

  /** Flatten a projection into a StructNode.
    * @param collapse If set to true, duplicate definitions are combined into a single one. This
    *   must not be used in the top-level Bind because the definitions have to match the top-level
    *   type (which is used later in `createResultSetMapping`). Any duplicates there will be
    *   eliminated in `hoistClientOps`. It is also disabled directly under a Union because the
    *   columns on both sides have to match up. */
  def flattenProjection(n: Node, collapse: Boolean): (StructNode, Map[List[TermSymbol], TermSymbol]) = {
    val defs = new ArrayBuffer[(TermSymbol, Node)]
    val defsM = new HashMap[Node, TermSymbol]
    val paths = new HashMap[List[TermSymbol], TermSymbol]
    def flatten(n: Node, path: List[TermSymbol]): Unit = {
      logger.debug("Flattening node at "+Path.toString(path), n)
      n match {
        case StructNode(ch) => ch.foreach { case (s, n) => flatten(n, s :: path) }
        case p: ProductNode =>
          p.children.zipWithIndex.foreach { case (n, i) => flatten(n, new ElementSymbol(i+1) :: path) }
        case n =>
          if(collapse) {
            defsM.get(n) match {
              case Some(sym) =>
                logger.debug(s"Reusing definition: $sym -> $n")
                paths += path -> sym
              case _ =>
                val sym = new AnonSymbol
                logger.debug(s"Adding definition: $sym -> $n")
                defs += sym -> n
                defsM += n -> sym
                paths += path -> sym
            }
          } else {
            val sym = new AnonSymbol
            logger.debug(s"Adding definition: $sym -> $n")
            defs += sym -> n
            paths += path -> sym
          }
      }
    }
    flatten(n, Nil)
    (StructNode(ConstArray.from(defs)), paths.toMap)
  }
}
