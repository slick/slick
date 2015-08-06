package slick.compiler

import slick.SlickException
import slick.ast._
import slick.ast.Util._
import slick.ast.TypeUtil._
import slick.util.{Ellipsis, ??}

import scala.util.control.NonFatal

/** Lift applicable operations at the top level to the client side. */
class HoistClientOps extends Phase {
  val name = "hoistClientOps"

  def apply(state: CompilerState) = state.map(ClientSideOp.mapResultSetMapping(_) { rsm =>
    val from1 = shuffle(rsm.from)
    from1 match {
      case Bind(s2, from2, Pure(StructNode(defs2), ts2)) =>
        // Extract client-side operations into ResultSetMapping
        val hoisted = defs2.map { case (ts, n) => (ts, n, unwrap(n)) }
        logger.debug("Hoisting operations from defs: " + hoisted.filter(t => t._2 ne t._3._1).map(_._1).mkString(", "))
        val newDefsM = hoisted.map { case (ts, n, (n2, wrap)) => (n2, new AnonSymbol) }.toMap
        val oldDefsM = hoisted.map { case (ts, n, (n2, wrap)) => (ts, wrap(Select(Ref(rsm.generator), newDefsM(n2)))) }.toMap
        val bind2 = rewriteDBSide(Bind(s2, from2, Pure(StructNode(newDefsM.map(_.swap).toVector), new AnonTypeSymbol)).infer())
        val rsm2 = rsm.copy(from = bind2, map = rsm.map.replace {
          case Select(Ref(s), f) if s == rsm.generator => oldDefsM(f)
        }).infer()
        logger.debug("New ResultSetMapping:", Ellipsis(rsm2, List(0, 0)))
        rsm2
      case _ =>
        val from2 = rewriteDBSide(from1)
        if(from2 eq rsm.from) rsm else rsm.copy(from = from2).infer()
    }
  })

  /** Pull Bind nodes up to the top level through Filter and CollectionCast. */
  def shuffle(n: Node): Node = n match {
    case n @ Bind(s1, from1, sel1) =>
      shuffle(from1) match {
        case bind2 @ Bind(s2, from2, sel2 @ Pure(StructNode(elems2), ts2)) if !from2.isInstanceOf[GroupBy] =>
          logger.debug("Merging top-level Binds", Ellipsis(n.copy(from = bind2), List(0,0)))
          val defs = elems2.toMap
          bind2.copy(select = sel1.replace {
            case Select(Ref(s), f) if s == s1 => defs(f)
          }).infer()
        case from2 =>
          if(from2 eq from1) n else n.copy(from = from2) :@ n.nodeType
      }

    // Push CollectionCast down unless it casts from a collection without duplicates to one with duplicates.
    //TODO: Identity mappings are reversible, to we can safely allow them for any kind of conversion.
    case n @ CollectionCast(from1 :@ CollectionType(cons1, _), cons2) if !cons1.isUnique || cons2.isUnique =>
      shuffle(from1) match {
        case Bind(s1, bfrom1, sel1 @ Pure(StructNode(elems1), ts1)) if !bfrom1.isInstanceOf[GroupBy] =>
          val res = Bind(s1, CollectionCast(bfrom1, cons2), sel1.replace { case Ref(s) if s == s1 => Ref(s) }).infer()
          logger.debug("Pulled Bind out of CollectionCast", Ellipsis(res, List(0,0)))
          res
        case from2 => if(from2 eq from1) n else n.copy(child = from2) :@ n.nodeType
      }

    case n @ Filter(s1, from1, pred1) =>
      shuffle(from1) match {
        case from2 @ Bind(bs1, bfrom1, sel1 @ Pure(StructNode(elems1), ts1)) if !bfrom1.isInstanceOf[GroupBy] =>
          logger.debug("Pulling Bind out of Filter", Ellipsis(n.copy(from = from2), List(0, 0)))
          val s3 = new AnonSymbol
          val defs = elems1.toMap
          val res = Bind(bs1, Filter(s3, bfrom1, pred1.replace {
            case Select(Ref(s), f) if s == s1 => defs(f).replace { case Ref(s) if s == bs1 => Ref(s3) }
          }), sel1.replace { case Ref(s) if s == bs1 => Ref(s) })
          logger.debug("Pulled Bind out of Filter", Ellipsis(res, List(0,0)))
          res.infer()
        case from2 =>
          if(from2 eq from1) n else n.copy(from = from2) :@ n.nodeType
      }

    case n => n
  }

  /** Remove a hoistable operation from a top-level column and create a function to
    * reapply it at the client side. */
  def unwrap(n: Node): (Node, (Node => Node)) = n match {
    case GetOrElse(ch, default) =>
      val (recCh, recTr) = unwrap(ch)
      (recCh, { sym => GetOrElse(recTr(sym), default) })
    case OptionApply(ch) =>
      val (recCh, recTr) = unwrap(ch)
      (recCh, { sym => OptionApply(recTr(sym)) })
    case n => (n, identity)
  }

  /** Rewrite remaining `GetOrElse` operations in the server-side tree into conditionals. */
  def rewriteDBSide(tree: Node): Node = tree.replace({
    case GetOrElse(OptionApply(ch), _) => ch
    case n @ GetOrElse(ch :@ OptionType(tpe), default) =>
      logger.debug("Translating GetOrElse to IfNull", n)
      val d = try default() catch {
        case NonFatal(ex) => throw new SlickException(
          "Caught exception while computing default value for Rep[Option[_]].getOrElse -- "+
            "This cannot be done lazily when the value is needed on the database side", ex)
      }
      Library.IfNull.typed(tpe, ch, LiteralNode(tpe, d)).infer()
  }, keepType = true, bottomUp = true)
}
