package scala.slick.ast
package opt

import Util._
import scala.slick.SLICKException
import scala.slick.util.Logging
import scala.collection.mutable.{HashSet, HashMap}

/**
 * Remove TableExpansions and TableRefExpansions, and flatten ProductNodes
 * into StructNodes and remove unnecessary columns from them.
 */
object PathRewriter extends (Node => Node) with Logging {

  def apply(n: Node): Node = {
    def flattenToStruct(n: Node): (Node, Vector[(Symbol, Node)]) = n match {
      case ProductNode(ch @ _*) =>
        val chf = ch.map(flattenToStruct)
        (ProductNode(chf.map(_._1): _*), chf.map(_._2).foldLeft[Vector[(Symbol, Node)]](Vector())(_ ++ _))
      case n =>
        val sym = new AnonSymbol
        (Ref(sym), Vector((sym, n)))
    }

    val flattened = new HashMap[List[Symbol], Node]
    val defs = new HashMap[Symbol, Node]

    def narrowRef(s: Symbol): Symbol = defs.get(s) match {
      case Some(u: Union) => narrowRef(u.leftGen)
      case Some(FilteredQuery(gen, _)) => narrowRef(gen)
      case _ => s
    }

    def findFlattened(syms: List[Symbol], base: List[Symbol]): Option[(Node, List[Symbol], List[Symbol])] = syms match {
      case Nil => None
      case h :: t =>
        val nh = narrowRef(h)
        defs.get(nh) match {
          case Some(j: Join) =>
            logger.debug("  found Join for "+nh+" (from "+h+")")
            t match {
              case (e @ ElementSymbol(1)) :: tt => findFlattened(j.leftGen :: tt, e :: base)
              case (e @ ElementSymbol(2)) :: tt => findFlattened(j.rightGen :: tt, e :: base)
              case _ => None
            }
          case o =>
            logger.debug("  found non-join for "+nh+" (from "+h+"): "+o)
            flattened.get(List(nh)).map(n => (n, t, base))
        }
    }

    /** Remove expansions, flatten structs, and gather defs and flattened structs */
    def gather(refO: Option[Symbol], n: Node): Node = removeExpansion(n) match {
      case Bind(gen, from, Pure(x)) =>
        val from2 = from match {
          case Pure(_) =>
            val x2 = gather(Some(gen), from).asInstanceOf[Pure].child
            val (mapping, repl) = flattenToStruct(x2)
            logger.debug("Storing flattened Pure struct as "+Path.toString(List(gen)))
            flattened += List(gen) -> mapping
            Pure(StructNode(repl))
          case n =>
            gather(Some(gen), n)
        }
        logger.debug("Storing def for "+gen)
        defs += gen -> from2
        val x2 = gather(None, x)
        val pure2 = refO match {
          case Some(ref) =>
            val (mapping, repl) = flattenToStruct(x2)
            logger.debug("Storing flattened struct as "+Path.toString(List(ref)))
            flattened += List(ref) -> mapping
            StructNode(repl)
          case None =>
            ProductNode(x2.flattenProduct: _*)
        }
        Bind(gen, from2, Pure(pure2))
      case d: DefNode =>
        d.nodeMapScopedChildren { case (symO, ch) =>
          val ch2 = gather(symO, ch)
          symO.foreach { sym =>
            logger.debug("Storing def for "+sym)
            defs += sym -> ch2
          }
          ch2
        }
      case n =>
        n.nodeMapChildren(ch => gather(None, ch))
    }

    def replaceRefs(n: Node): Node = n match {
      case PathOrRef(syms) => syms.head match {
        case f: FieldSymbol => n // inside a former TableExpansion - no need to go down this path
        case _ =>
          logger.debug("Trying to replace "+Path.toString(syms))
          val rsyms = syms.reverse
          findFlattened(rsyms, Nil) match {
            case Some(fl @ (struct, rest, base)) =>
              logger.debug("  found flattened: "+fl)
              val fsym = findFieldSymbol(struct, rest)
              Path(fsym :: base ::: rsyms.head :: Nil)
            case _ => n
          }
      }
      case n => n.nodeMapChildren(replaceRefs)
    }

    val n2 = gather(None, n)
    if(n2 ne n) logger.debug("Expansions removed, ProductsNodes rewritten to StructNodes", n2)
    val n3 = replaceRefs(n2)
    prune.repeat(n3)
  }

  def findFieldSymbol(n: Node, path: List[Symbol]): Symbol = (path, n) match {
    case (Nil, Ref(sym)) => sym
    case (ElementSymbol(idx) :: t, ProductNode(ch @ _*)) => findFieldSymbol(ch(idx-1), t)
    case _ => throw new SLICKException("Illegal path "+Path.toString(path)+"into TableExpansion structure")
  }

  def removeExpansion(n: Node) = n match {
    case TableExpansion(gen, t, cols) => Bind(gen, t, Pure(cols))
    case TableRefExpansion(_, _, cols) => cols
    case n => n
  }

  val prune = new Transformer {
    val refs = new HashSet[Symbol]()
    override def initTree(n: Node) {
      super.initTree(n)
      refs.clear()
      refs ++= n.collect[Symbol] { case Select(_, f: Symbol) => f }
      logger.debug("Protecting refs: "+refs)
    }
    def replace = {
      case n @ StructNode(ch) =>
        val ch2 = ch.filter { case (sym, n) => refs.contains(sym) }
        if(ch2.length == ch.length) n else StructNode(ch2)
    }
  }
}
