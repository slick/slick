package slick.compiler

import scala.util.control.NonFatal

import slick.SlickException
import slick.ast.*
import slick.ast.TypeUtil.*
import slick.ast.Util.*
import slick.util.{ConstArray, Ellipsis}

/** Lift applicable operations at the top level to the client side. */
class HoistClientOps extends Phase {
  val name = "hoistClientOps"

  def apply(state: CompilerState) = state.map(ClientSideOp.mapResultSetMapping(_) { rsm =>
    val from1 = shuffle(rsm.from)
    from1 match {
      case Bind(s2, from2, Pure(StructNode(defs2), ts2)) =>
        // Extract client-side operations into ResultSetMapping
        val hoisted = defs2.map { case (ts, n) => (ts, n, unwrap(n, topLevel = true)) }
        logger.debug("Hoisting operations from defs: " + hoisted.iterator.filter(t => t._2 ne t._3._1).map(_._1).mkString(", "))
        val newDefsM = hoisted.iterator.zipWithIndex.map { case ((ts, n, (n2, wrap)), idx) => (idx, (n2, new AnonSymbol)) }.toMap
        logger.debug("New defs: "+newDefsM)
        val oldDefsM = hoisted.iterator.zipWithIndex.map { case ((ts, n, (n2, wrap)), idx) => (ts, wrap(Select(Ref(rsm.generator), newDefsM(idx)._2))) }.toMap
        val bind2 = rewriteDBSide(Bind(s2, from2, Pure(StructNode(ConstArray.from(newDefsM.map(_._2.swap))), new AnonTypeSymbol)).infer())
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
        // Merge nested Binds
        case bind2 @ Bind(s2, from2, sel2 @ Pure(StructNode(elems2), ts2)) if !from2.isInstanceOf[GroupBy] =>
          logger.debug("Merging top-level Binds", Ellipsis(n.copy(from = bind2), List(0,0)))
          val defs = elems2.iterator.toMap
          bind2.copy(select = sel1.replace {
            case Select(Ref(s), f) if s == s1 => defs(f)
          }).infer()
        // Hoist operations out of the non-Option sides of inner and left and right outer joins
        case from2 @ Join(sl1, sr1, bl @ Bind(bsl, lfrom, Pure(StructNode(ldefs), tsl)),
                                    br @ Bind(bsr, rfrom, Pure(StructNode(rdefs), tsr)),
                          jt, on1) if jt != JoinType.Outer =>
          logger.debug("Hoisting operations from Join:", Ellipsis(from2, List(0, 0), List(1, 0)))
          val (bl2: Bind, lrepl: Map[TermSymbol, (Node => Node, AnonSymbol)] @unchecked) = if(jt != JoinType.Right) {
            val hoisted = ldefs.map { case (ts, n) => (ts, n, unwrap(n, false)) }
            logger.debug("Hoisting operations from defs in left side of Join: " + hoisted.iterator.filter(t => t._2 ne t._3._1).map(_._1).mkString(", "))
            val newDefsM = hoisted.iterator.map { case (ts, n, (n2, wrap)) => (n2, new AnonSymbol) }.toMap
            logger.debug("New defs: "+newDefsM)
            val bl2 = bl.copy(select = Pure(StructNode(ConstArray.from(newDefsM.map(_.swap))))).infer()
            logger.debug("Translated left join side:", Ellipsis(bl2, List(0)))
            val repl = hoisted.iterator.map { case (s, _, (n2, wrap)) => (s, (wrap, newDefsM(n2))) }.toMap
            (bl2, repl)
          } else (bl, Map.empty)
          val (br2: Bind, rrepl: Map[TermSymbol, (Node => Node, AnonSymbol)] @unchecked) = if(jt != JoinType.Left) {
            val hoisted = rdefs.map { case (ts, n) => (ts, n, unwrap(n, false)) }
            logger.debug("Hoisting operations from defs in right side of Join: " + hoisted.iterator.filter(t => t._2 ne t._3._1).map(_._1).mkString(", "))
            val newDefsM = hoisted.iterator.map { case (ts, n, (n2, wrap)) => (n2, new AnonSymbol) }.toMap
            logger.debug("New defs: "+newDefsM)
            val br2 = br.copy(select = Pure(StructNode(ConstArray.from(newDefsM.map(_.swap))))).infer()
            logger.debug("Translated right join side:", Ellipsis(br2, List(0)))
            val repl = hoisted.iterator.map { case (s, _, (n2, wrap)) => (s, (wrap, newDefsM(n2))) }.toMap
            (br2, repl)
          } else (br, Map.empty)
          if((bl2 ne bl) || (br2 ne br)) {
            val from3 = from2.copy(left = bl2, right = br2, on = on1.replace {
              case Select(Ref(s), f) if s == sl1 && (bl2 ne bl) =>
                val (wrap, f2) = lrepl(f)
                wrap(Select(Ref(s), f2))
              case Select(Ref(s), f) if s == sr1 && (br2 ne br) =>
                val (wrap, f2) = rrepl(f)
                wrap(Select(Ref(s), f2))
              case Ref(s) if (s == sl1 && (bl2 ne bl)) || (s == sr1 && (br2 ne br)) =>
                Ref(s)
            })
            val sel2 = sel1.replace {
              case Select(Select(Ref(s), ElementSymbol(1)), f) if s == s1 && (bl2 ne bl) =>
                val (wrap, f2) = lrepl(f)
                wrap(Select(Select(Ref(s), ElementSymbol(1)), f2))
              case Select(Select(Ref(s), ElementSymbol(2)), f) if s == s1 && (br2 ne br) =>
                val (wrap, f2) = rrepl(f)
                wrap(Select(Select(Ref(s), ElementSymbol(2)), f2))
              case Ref(s) if s == s1 => Ref(s)
            }
            logger.debug("from3", from3)
            logger.debug("sel2", sel2)
            n.copy(from = from3, select = sel2).infer()
          } else if(from2 eq from1) n
          else n.copy(from = from2) :@ n.nodeType
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
          val defs = elems1.iterator.toMap
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

  /** Remove a hoistable operation from a top-level column or join column and create a
    * function to reapply it at an outer layer. */
  def unwrap(n: Node, topLevel: Boolean): (Node, (Node => Node)) = n match {
    case GetOrElse(ch, default) =>
      val (recCh, recTr) = unwrap(ch, topLevel)
      (recCh, { sym => GetOrElse(recTr(sym), default) })
    case OptionApply(ch) =>
      val (recCh, recTr) = unwrap(ch, topLevel)
      (recCh, { sym => OptionApply(recTr(sym)) })
    case IfThenElse(ConstArray(Library.==(ch, LiteralNode(null)), r1 @ LiteralNode(None), r2 @ LiteralNode(Some(1)))) :@ OptionType(t)
        if t == ScalaBaseType.optionDiscType =>
      val (recCh, recTr) = unwrap(ch, topLevel)
      if(topLevel) (recCh, recTr)
      else (recCh, { n => IfThenElse(ConstArray(Library.==.typed[Boolean](recTr(n), LiteralNode(null)), r1, r2)) })
    case Library.SilentCast(ch) :@ tpe if !topLevel =>
      val (recCh, recTr) = unwrap(ch, topLevel)
      (recCh, { n => Library.SilentCast.typed(tpe, recTr(n)) })
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
