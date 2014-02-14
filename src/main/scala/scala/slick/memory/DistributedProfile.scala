package scala.slick.memory

import scala.language.{implicitConversions, existentials}
import scala.collection.mutable.HashMap
import scala.slick.SlickException
import scala.slick.ast._
import scala.slick.compiler._
import scala.slick.profile.{RelationalDriver, RelationalProfile}
import TypeUtil.typeToTypeUtil

/** A profile and driver for distributed queries. */
trait DistributedProfile extends MemoryQueryingProfile { driver: DistributedDriver =>
  val drivers: Seq[RelationalProfile]

  type Backend = DistributedBackend
  type QueryExecutor[R] = QueryExecutorDef[R]
  val backend: Backend = DistributedBackend
  val simple: SimpleQL = new SimpleQL {}
  val Implicit: Implicits = simple

  lazy val queryCompiler =
    QueryCompiler.standard.addAfter(new Distribute, Phase.assignUniqueSymbols) + new MemoryCodeGen
  lazy val updateCompiler = ???
  lazy val deleteCompiler = ???
  lazy val insertCompiler = ???

  def createQueryExecutor[R](tree: Node, param: Any): QueryExecutor[R] = new QueryExecutorDef[R](tree, param)
  def createInsertInvoker[T](tree: scala.slick.ast.Node): InsertInvoker[T] = ???
  def buildSequenceSchemaDescription(seq: Sequence[_]): SchemaDescription = ???
  def buildTableSchemaDescription(table: Table[_]): SchemaDescription = ???
  def createDistributedQueryInterpreter(param: Any, session: Backend#Session) = new DistributedQueryInterpreter(param, session)
  def createDDLInvoker(sd: SchemaDescription): DDLInvoker = ???

  val emptyHeapDB = HeapBackend.createEmptyDatabase

  trait SimpleQL extends super.SimpleQL with Implicits

  trait Implicits extends super.Implicits {
    implicit def ddlToDDLInvoker(d: SchemaDescription): DDLInvoker = ???
  }

  class QueryExecutorDef[R](tree: Node, param: Any) extends super.QueryExecutorDef[R] {
    def run(implicit session: Backend#Session): R =
      createDistributedQueryInterpreter(param, session).run(tree).asInstanceOf[R]
  }

  class DistributedQueryInterpreter(param: Any, session: Backend#Session) extends QueryInterpreter(emptyHeapDB, param) {
    import QueryInterpreter._

    override def run(n: Node) = n match {
      case DriverComputation(compiled, driver, _) =>
        if(logger.isDebugEnabled) logDebug("Evaluating "+n)
        val idx = drivers.indexOf(driver)
        if(idx < 0) throw new SlickException("No session found for driver "+driver)
        val driverSession = session.sessions(idx).asInstanceOf[driver.Backend#Session]
        val dv = driver.createQueryExecutor[Any](compiled, param).run(driverSession)
        val wr = wrapScalaValue(dv, n.nodeType)
        if(logger.isDebugEnabled) logDebug("Wrapped value: "+wr)
        wr
      case ResultSetMapping(gen, from, CompiledMapping(converter, tpe)) =>
        if(logger.isDebugEnabled) logDebug("Evaluating "+n)
        val fromV = run(from).asInstanceOf[TraversableOnce[Any]]
        val b = n.nodeType.asCollectionType.cons.createErasedBuilder
        b ++= fromV.map(v => converter.read(v.asInstanceOf[QueryInterpreter.ProductValue]))
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
}

class DistributedDriver(val drivers: RelationalProfile*) extends MemoryQueryingDriver with DistributedProfile { driver =>
  override val profile: DistributedProfile = this

  /** Compile sub-queries with the appropriate drivers */
  class Distribute extends Phase {
    import Util._
    val name = "distribute"

    def apply(state: CompilerState) = state.map { tree =>
      // Collect the required drivers and tainting drivers for all subtrees
      val needed = new HashMap[IntrinsicSymbol, Set[RelationalDriver]]
      val taints = new HashMap[IntrinsicSymbol, Set[RelationalDriver]]
      def collect(n: Node, scope: Scope): (Set[RelationalDriver], Set[RelationalDriver]) = {
        val (dr: Set[RelationalDriver], tt: Set[RelationalDriver]) = (n match {
          case t: TableNode => (Set(t.driverTable.asInstanceOf[RelationalDriver#Table[_]].tableProvider), Set.empty)
          case Ref(sym) =>
            scope.get(sym) match {
              case Some(nn) =>
                val target = nn._1.nodeIntrinsicSymbol
                (Set.empty, needed(target) ++ taints(target))
              case None =>
                (Set.empty, Set.empty)
            }
          case n =>
            var nnd = Set.empty[RelationalDriver]
            var ntt = Set.empty[RelationalDriver]
            n.mapChildrenWithScope({ (_, n, sc) =>
              val (nd, tt) = collect(n, sc)
              nnd ++= nd
              ntt ++= tt
              n
            }, scope)
            (nnd, ntt)
        })
        needed += n.nodeIntrinsicSymbol -> dr
        taints += n.nodeIntrinsicSymbol -> tt
        (dr, tt)
      }
      collect(tree, Scope.empty)
      def transform(n: Node): Node = {
        val dr = needed(n.nodeIntrinsicSymbol)
        val tt = taints(n.nodeIntrinsicSymbol)
        if(dr.size == 1 && (tt -- dr).isEmpty) {
          val compiled = dr.head.queryCompiler.run(n).tree
          DriverComputation(compiled, dr.head, compiled.nodeType)
        } else n.nodeMapChildren(transform)
      }
      transform(tree)
    }
  }
}

/** Represents a computation that needs to be performed by another driver.
  * Despite having a child it is a NullaryNode because the sub-computation
  * should be opaque to the query compiler. */
final case class DriverComputation(val compiled: Node, val driver: RelationalDriver, tpe: Type) extends NullaryNode with TypedNode {
  type Self = DriverComputation
  protected[this] def nodeRebuild = copy()
  override def toString = s"DriverComputation($driver)"
}
