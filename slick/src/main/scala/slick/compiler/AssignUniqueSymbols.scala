package slick.compiler

import slick.ast.Library.AggregateFunctionSymbol

import scala.collection.mutable.HashMap
import slick.ast._

/** Ensure that all symbol definitions in a tree are unique. The same symbol can initially occur in
  * multiple sub-trees when some part of a query is reused multiple times. This phase assigns new,
  * uniqe symbols, so that later phases do not have to take scopes into account for identifying the
  * source of a symbol. The rewriting is performed for both, term symbols and type symbols.
  *
  * The phase state is a collection of flags depending on the presence or absence of certain node
  * types in the AST. This information can be used to selectively skip later compiler phases when
  * it is already known that there is nothing for them to translate.
  */
class AssignUniqueSymbols extends Phase {
  val name = "assignUniqueSymbols"

  type State = UsedFeatures

  def apply(state: CompilerState) = {
    var hasDistinct, hasTypeMapping, hasAggregate, hasNonPrimitiveOption = false
    val s2 = state.map { tree =>
      val replace = new HashMap[TermSymbol, AnonSymbol]
      def checkFeatures(n: Node): Unit = n match {
        case _: Distinct => hasDistinct = true
        case _: TypeMapping => hasTypeMapping = true
        case n: Apply =>
          if(n.sym.isInstanceOf[AggregateFunctionSymbol]) hasAggregate = true
        case (_: OptionFold | _: OptionApply | _: GetOrElse) => hasNonPrimitiveOption = true
        case j: Join =>
          if(j.jt == JoinType.LeftOption || j.jt == JoinType.RightOption || j.jt == JoinType.OuterOption) hasNonPrimitiveOption = true
        case _ =>
      }
      def tr(n: Node): Node = {
        val n3 = n match {
          case Select(in, s) => Select(tr(in), s) :@ n.nodeType
          case r @ Ref(a: AnonSymbol) =>
            val s = replace.getOrElse(a, a)
            if(s eq a) r else Ref(s)
          case t: TableNode => t.copy(identity = new AnonTableIdentitySymbol)(t.profileTable)
          case Pure(value, _) => Pure(tr(value))
          case g: GroupBy =>
            val d = g.copy(identity = new AnonTypeSymbol)
            val a = new AnonSymbol
            replace += g.fromGen -> a
            g.copy(fromGen = a, tr(g.from), tr(g.by), identity = new AnonTypeSymbol)
          case n: StructNode => n.mapChildren(tr)
          case d: DefNode =>
            checkFeatures(d)
            replace ++= d.generators.iterator.map(_._1 -> new AnonSymbol)
            d.mapSymbols(s => replace.getOrElse(s, s)).mapChildren(tr)
          case n =>
            checkFeatures(n)
            n.mapChildren(tr)
        }
        // Remove all NominalTypes (which might have changed)
        if(n3.hasType && hasNominalType(n3.nodeType)) n3.untyped else n3
      }
      tr(tree)
    }
    val features = UsedFeatures(hasDistinct, hasTypeMapping, hasAggregate, hasNonPrimitiveOption)
    logger.debug("Detected features: "+features)
    s2 + (this -> features)
  }

  def hasNominalType(t: Type): Boolean = t match {
    case _: NominalType => true
    case _: AtomicType => false
    case _ => t.children.exists(hasNominalType)
  }
}

case class UsedFeatures(distinct: Boolean, typeMapping: Boolean, aggregate: Boolean, nonPrimitiveOption: Boolean)
