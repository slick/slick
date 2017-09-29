package slick.memory

import scala.language.{implicitConversions, existentials}

import scala.collection.mutable.{Builder, HashMap}

import slick.SlickException
import slick.ast._
import slick.ast.TypeUtil._
import slick.basic.{FixedBasicAction, FixedBasicStreamingAction}
import slick.compiler._
import slick.dbio._
import slick.relational.{RelationalProfile, ResultConverter, CompiledMapping}
import slick.util.{DumpInfo, RefId, ??}

/** A profile for distributed queries. */
class DistributedProfile(val profiles: RelationalProfile*) extends MemoryQueryingProfile { self: DistributedProfile =>

  @deprecated("Use the Profile object directly instead of calling `.profile` on it", "3.2")
  override val profile: DistributedProfile = this

  type Backend = DistributedBackend
  type QueryExecutor[R] = QueryExecutorDef[R]
  val backend: Backend = DistributedBackend
  val api: API = new API {}

  lazy val queryCompiler =
    QueryCompiler.standard.addAfter(new Distribute, Phase.assignUniqueSymbols) ++ QueryCompiler.interpreterPhases + new MemoryCodeGen
  lazy val updateCompiler = ??
  lazy val deleteCompiler = ??
  lazy val insertCompiler = ??

  def createQueryExecutor[R](tree: Node, param: Any): QueryExecutor[R] = new QueryExecutorDef[R](tree, param)
  def createDistributedQueryInterpreter(param: Any, session: Backend#Session) = new DistributedQueryInterpreter(param, session)

  type QueryActionExtensionMethods[R, S <: NoStream] = QueryActionExtensionMethodsImpl[R, S]
  type StreamingQueryActionExtensionMethods[R, T] = StreamingQueryActionExtensionMethodsImpl[R, T]

  def createQueryActionExtensionMethods[R, S <: NoStream](tree: Node, param: Any): QueryActionExtensionMethods[R, S] =
    new QueryActionExtensionMethods[R, S](tree, param)
  def createStreamingQueryActionExtensionMethods[R, T](tree: Node, param: Any): StreamingQueryActionExtensionMethods[R, T] =
    new StreamingQueryActionExtensionMethods[R, T](tree, param)

  val emptyHeapDB = HeapBackend.createEmptyDatabase

  class QueryExecutorDef[R](tree: Node, param: Any) {
    def run(implicit session: Backend#Session): R =
      createDistributedQueryInterpreter(param, session).run(tree).asInstanceOf[R]
  }

  type ProfileAction[+R, +S <: NoStream, -E <: Effect] = FixedBasicAction[R, S, E]
  type StreamingProfileAction[+R, +T, -E <: Effect] = FixedBasicStreamingAction[R, T, E]

  class QueryActionExtensionMethodsImpl[R, S <: NoStream](tree: Node, param: Any) extends super.QueryActionExtensionMethodsImpl[R, S] {
    protected[this] val exe = createQueryExecutor[R](tree, param)
    def result: ProfileAction[R, S, Effect.Read] =
      new StreamingProfileAction[R, Any, Effect.Read] with SynchronousDatabaseAction[R, Streaming[Any], Backend#This, Effect.Read] {
        def run(ctx: Backend#Context) = exe.run(ctx.session)
        def getDumpInfo = DumpInfo("DistributedProfile.ProfileAction")
        def head: ResultAction[Any, NoStream, Effect.Read] = ??
        def headOption: ResultAction[Option[Any], NoStream, Effect.Read] = ??
      }.asInstanceOf[ProfileAction[R, S, Effect.Read]]
  }

  class StreamingQueryActionExtensionMethodsImpl[R, T](tree: Node, param: Any) extends QueryActionExtensionMethodsImpl[R, Streaming[T]](tree, param) with super.StreamingQueryActionExtensionMethodsImpl[R, T] {
    override def result: StreamingProfileAction[R, T, Effect.Read] = super.result.asInstanceOf[StreamingProfileAction[R, T, Effect.Read]]
  }

  class DistributedQueryInterpreter(param: Any, session: Backend#Session) extends QueryInterpreter(emptyHeapDB, param) {
    import QueryInterpreter._

    override def run(n: Node) = n match {
      case ProfileComputation(compiled, profile, _) =>
        if(logger.isDebugEnabled) logDebug("Evaluating "+n)
        val idx = profiles.indexOf(profile)
        if(idx < 0) throw new SlickException("No session found for profile "+profile)
        val profileSession = session.sessions(idx).asInstanceOf[profile.Backend#Session]
        val dv = profile.runSynchronousQuery[Any](compiled, param)(profileSession)
        val wr = wrapScalaValue(dv, n.nodeType)
        if(logger.isDebugEnabled) logDebug("Wrapped value: "+wr)
        wr
      case ResultSetMapping(gen, from, CompiledMapping(converter, tpe)) :@ CollectionType(cons, el) =>
        if(logger.isDebugEnabled) logDebug("Evaluating "+n)
        val fromV = run(from).asInstanceOf[TraversableOnce[Any]]
        val b = cons.createBuilder(el.classTag).asInstanceOf[Builder[Any, Any]]
        b ++= fromV.map(v => converter.asInstanceOf[ResultConverter[MemoryResultConverterDomain, Any]].read(v.asInstanceOf[QueryInterpreter.ProductValue]))
        b.result()
      case n => super.run(n)
    }

    def wrapScalaValue(value: Any, tpe: Type): Any = tpe match {
      case ProductType(ts) =>
        val p = value.asInstanceOf[Product]
        new ProductValue((0 until p.productArity).map(i =>
          wrapScalaValue(p.productElement(i), ts(i))
        )(collection.breakOut))
      case CollectionType(_, elType) =>
        val v = value.asInstanceOf[Traversable[_]]
        val b = v.companion.newBuilder[Any]
        v.foreach(v => b += wrapScalaValue(v, elType))
        b.result()
      case _ => value
    }
  }

  /* internal: */

  /** Compile sub-queries with the appropriate profile */
  class Distribute extends Phase {
    import Util._
    val name = "distribute"

    def apply(state: CompilerState) = state.map { tree =>
      // Collect the required profiles and tainting profiles for all subtrees
      val needed = new HashMap[RefId[Node], Set[RelationalProfile]]
      val taints = new HashMap[RefId[Node], Set[RelationalProfile]]
      def collect(n: Node, scope: Scope): (Set[RelationalProfile], Set[RelationalProfile]) = {
        val (dr: Set[RelationalProfile], tt: Set[RelationalProfile]) = n match {
          case t: TableNode => (Set(t.profileTable.asInstanceOf[RelationalProfile#Table[_]].tableProvider), Set.empty)
          case Ref(sym) =>
            scope.get(sym) match {
              case Some(nn) =>
                val target = RefId(nn._1)
                (Set.empty, needed(target) ++ taints(target))
              case None =>
                (Set.empty, Set.empty)
            }
          case n =>
            var nnd = Set.empty[RelationalProfile]
            var ntt = Set.empty[RelationalProfile]
            mapChildrenWithScope(n, { (n, sc) =>
              val (nd, tt) = collect(n, sc)
              nnd ++= nd
              ntt ++= tt
              n
            }, scope)
            (nnd, ntt)
        }
        needed += RefId(n) -> dr
        taints += RefId(n) -> tt
        (dr, tt)
      }
      collect(tree, Scope(Map()))
      def transform(n: Node): Node = {
        val dr = needed(RefId(n))
        val tt = taints(RefId(n))
        if(dr.size == 1 && (tt -- dr).isEmpty) {
          val compiled = dr.head.queryCompiler.run(n).tree
          val substituteType = compiled.nodeType.replace {
            case CollectionType(cons, el) => CollectionType(cons.iterableSubstitute, el)
          }
          ProfileComputation(compiled :@ substituteType, dr.head, substituteType)
        } else n.mapChildren(transform)
      }
      transform(tree)
    }

    def mapChildrenWithScope(tree: Node, f: (Node, Scope) => Node, scope: Scope): Node = tree match {
      case d: DefNode =>
        var local = scope
        d.mapScopedChildren { (symO, ch) =>
          val r = f(ch, local)
          symO.foreach(sym => local = local + (sym, r))
          r
        }
      case n => n.mapChildren(ch => f(ch, scope))
    }

    case class Scope(m: Map[TermSymbol, (Node, Scope)]) {
      def get(s: TermSymbol) = m.get(s)
      def + (s: TermSymbol, n: Node) = Scope(m + (s -> (n, this)))
    }
  }
}

/** Represents a computation that needs to be performed by another profile.
  * Despite having a child it is a NullaryNode because the sub-computation
  * should be opaque to the query compiler. */
final case class ProfileComputation(compiled: Node, profile: RelationalProfile, buildType: Type) extends NullaryNode with SimplyTypedNode {
  type Self = ProfileComputation
  protected[this] def rebuild = copy()
  override def getDumpInfo = super.getDumpInfo.copy(mainInfo = profile.toString)
}
