package slick.compiler

import scala.collection.mutable.{ArrayBuffer, HashMap}
import slick.ast._
import Util._
import TypeUtil._

/** Flatten all Pure node contents into a single StructNode. */
class FlattenProjections extends Phase {
  val name = "flattenProjections"

  def apply(state: CompilerState) = state.map { tree =>
    val translations = new HashMap[TypeSymbol, (Map[List[Symbol], Symbol], StructType)]
    def tr(n: Node): Node = n match {
      case Pure(v, ts) =>
        logger.debug(s"Flattening projection $ts")
        val (newV, newTranslations) = flattenProjection(tr(v))
        translations += ts -> (newTranslations, newV.nodeType.asInstanceOf[StructType])
        val res = Pure(newV, ts).nodeWithComputedType()
        logger.debug("Flattened projection to", res)
        res
      case p @ Path(path) =>
        logger.debug("Analyzing "+Path.toString(path)+" with symbols "+translations.keySet.mkString(", "), p)
        splitPath(n, translations.keySet) match {
          case Some((base, rest, tsym)) =>
            logger.debug("Found "+Path.toString(path)+" with local part "+rest.map(Path.toString _)+" over "+tsym)
            val (paths, tpe) = translations(tsym)
            logger.debug(s"  Translation for $tsym: ($paths, $tpe)")
            def retype(n: Node): Node = n.nodeMapChildren(retype, keepType = true).nodeTypedOrCopy(n.nodeType.replace {
              case t @ NominalType(tsym, _) if translations.contains(tsym) => t.withStructuralView(tpe)
            })
            rest match {
              case Some(r) => Select(retype(base), paths(r)).nodeWithComputedType()
              case None => retype(base)
            }
          case None => n
        }
      case n => n.nodeMapChildren(tr)
    }
    tr(tree).nodeWithComputedType(typeChildren = true)
  }

  /** Split a path into the shortest part with a NominalType and the rest on
    * top of it. Returns `None` if there is no NominalType with one of the
    * candidate TypeSymbols in the path, otherwise returns `Some(base, rest, tsym)`
    * where `tsym` is the symbol that was found and `base` is the Node in the path
    * which has a NominalType of that symbol. If the NominalType occurs inside a
    * CollectionType, `rest` is `None`, otherwise it is `Some(path)` with a
    * (possibly empty) path of symbols on top of `base`. */
  def splitPath(n: Node, candidates: scala.collection.Set[TypeSymbol]): Option[(Node, Option[List[Symbol]], TypeSymbol)] = {
    def checkType(tpe: Type): Option[(Node, Option[List[Symbol]], TypeSymbol)] = tpe match {
      case NominalType(tsym, _) if candidates contains tsym => Some((n, Some(Nil), tsym))
      case CollectionType(cons, el) => checkType(el).map { case (n, _, tsym) => (n, None, tsym) }
      case _ => None
    }
    n match {
      case Select(in, field) => splitPath(in, candidates) match {
        case Some((n, p, tsym)) => Some((n, Some(field :: p.get), tsym))
        case None => checkType(n.nodeType)
      }
      case _: Ref => checkType(n.nodeType)
    }
  }

  /** Flatten a projection into a StructNode. */
  def flattenProjection(n: Node): (StructNode, Map[List[Symbol], Symbol]) = {
    val defs = new ArrayBuffer[(Symbol, Node)]
    val paths = new HashMap[List[Symbol], Symbol]
    def flatten(n: Node, path: List[Symbol]) {
      logger.debug("Flattening node at "+Path.toString(path), n)
      n match {
        case StructNode(ch) => ch.foreach { case (s, n) => flatten(n, s :: path) }
        case p: ProductNode => p.numberedElements.foreach { case (s, n) => flatten(n, s :: path) }
        case n =>
          val sym = new AnonSymbol
          logger.debug(s"Adding definition: $sym -> $n")
          defs += sym -> n
          paths += path -> sym
      }
    }
    flatten(n, Nil)
    (StructNode(defs).nodeWithComputedType(), paths.toMap)
  }
}
