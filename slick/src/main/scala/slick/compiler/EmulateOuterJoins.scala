package slick.compiler

import slick.ast._
import Util._
import TypeUtil._
import slick.util.ConstArray

/** An optional phase which rewrites outer joins into more commonly supported
  * operations for use on databases that lack outer join support.
  *
  * Full outer joins are always emulated. Right joins can be replaced by left
  * joins (or an emulated version thereof). Left joins can be emulated with
  * inner joins and unions.
  */
class EmulateOuterJoins(val useLeftJoin: Boolean, val useRightJoin: Boolean) extends Phase {
  val name = "emulateOuterJoins"

  def apply(state: CompilerState) = state.map(tree => ClientSideOp.mapServerSide(tree, true){ n =>
    val n2 = convert(n)
    if(n2 eq n) n2 else Phase.forceOuterBinds.apply(n2)
  })

  def convert(n: Node): Node = n match {
    case Join(leftGen, rightGen, left, right, JoinType.Left, on) if !useLeftJoin =>
      // as leftJoin bs on e => (as join bs on e) unionAll as.filter(a => !exists(bs.filter(b => e(a, b)))).map(a => (a, nulls))
      val lgen2, rgen2, bgen = new AnonSymbol
      val on2 = on.replace({ case r @ Ref(sym) =>
        if(sym == leftGen) Ref(lgen2) else if(sym == rightGen) Ref(rgen2) else r
      }, true)
      convert(Union(
        Join(leftGen, rightGen, left, right, JoinType.Inner, on),
        Bind(bgen,
          Filter(lgen2, assignFreshSymbols(left),
            Library.Not.typed(on.nodeType, Library.Exists.typed(on.nodeType, Filter(rgen2, assignFreshSymbols(right), on2)))
          ),
          Pure(ProductNode(ConstArray(Ref(bgen), nullStructFor(right.nodeType.structural.asCollectionType.elementType))))
        ), true).infer())
    case Join(leftGen, rightGen, left, right, JoinType.Right, on) if !useRightJoin =>
      // as rightJoin bs on e => bs leftJoin as on { (b, a) => e(a, b) } map { case (b, a) => (a, b) }
      val bgen = new AnonSymbol
      convert(Bind(bgen,
        Join(rightGen, leftGen, right, left, JoinType.Left, on),
        Pure(ProductNode(ConstArray(Select(Ref(bgen), ElementSymbol(2)), Select(Ref(bgen), ElementSymbol(1)))))
      ).infer())
    case Join(leftGen, rightGen, left, right, JoinType.Outer, on) =>
      // as fullJoin bs on e => (as leftJoin bs on e) unionAll bs.filter(b => !exists(as.filter(a => e(a, b)))).map(b => (nulls, b))
      val lgen2, rgen2, bgen = new AnonSymbol
      val on2 = on.replace({ case r @ Ref(sym) =>
        if(sym == leftGen) Ref(lgen2) else if(sym == rightGen) Ref(rgen2) else r
      }, true)
      convert(Union(
        Join(leftGen, rightGen, left, right, JoinType.Left, on),
        Bind(bgen,
          Filter(rgen2, assignFreshSymbols(right),
            Library.Not.typed(on.nodeType, Library.Exists.typed(on.nodeType, Filter(lgen2, assignFreshSymbols(left), on2)))
          ),
          Pure(ProductNode(ConstArray(nullStructFor(left.nodeType.structural.asCollectionType.elementType), Ref(bgen))))
        ), true).infer())
    case n => n.mapChildren(convert, true)
  }

  /** Create a structure of the given type where all columns are NULL. */
  def nullStructFor(t: Type): Node = t.structural match {
    case ProductType(ts) => ProductNode(ts.map(nullStructFor))
    case StructType(sts) => StructNode(sts.map { case (s, t) => (s, nullStructFor(t)) })
    case t: OptionType => LiteralNode(t, None)
    case t => LiteralNode(OptionType(t), None)
  }

  /** Assign new TypeSymbols to a subtree that needs to be copied into multiple places. */
  def assignFreshSymbols(n: Node): Node = {
    val typeSyms = n.collect { case n: TypeGenerator => n.identity }.toSet
    val repl = typeSyms.map {
      case ts: TableIdentitySymbol => ts -> new AnonTableIdentitySymbol
      case ts => ts -> new AnonTypeSymbol
    }.toMap
    def replaceTS(t: Type): Type = (t match {
      case NominalType(ts, v) => repl.get(ts).map(new NominalType(_, v)).getOrElse(t)
      case t => t
    }).mapChildren(replaceTS)
    //repl.foreach { case (ts1, ts2) => global.get(ts1).foreach(t => global += ts2 -> replaceTS(t)) }
    n.replace({
      case n: TableNode => n.copy(identity = repl(n.identity).asInstanceOf[TableIdentitySymbol])(n.profileTable) :@ replaceTS(n.nodeType)
      case n: Pure => n.copy(identity = repl(n.identity))
      case n: GroupBy => n.copy(identity = repl(n.identity))
    }, bottomUp = true).infer()
  }
}
