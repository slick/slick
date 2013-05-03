package scala.slick.memory

import scala.language.{implicitConversions, existentials}
import scala.slick.ast._
import scala.slick.compiler._
import scala.slick.profile.{RelationalDriver, RelationalProfile}
import TypeUtil.typeToTypeUtil
import scala.slick.SlickException

/** A profile and driver for distributed queries. */
trait DistributedProfile extends MemoryQueryingProfile { driver: DistributedDriver =>
  val drivers: Seq[RelationalProfile]

  type Backend = DistributedBackend
  type QueryExecutor[R] = QueryExecutorDef[R]
  val backend: Backend = DistributedBackend
  val Implicit: Implicits = new Implicits {}
  val simple: SimpleQL = new SimpleQL {}

  lazy val queryCompiler =
    QueryCompiler.standard.addAfter(new Distribute, Phase.assignUniqueSymbols) + new MemoryCodeGen
  lazy val insertCompiler = ???

  def createQueryExecutor[R](tree: Node, param: Any): QueryExecutor[R] = new QueryExecutorDef[R](tree, param)
  def createInsertInvoker[T](tree: scala.slick.ast.Node): InsertInvoker[T] = ???
  def buildSequenceSchemaDescription(seq: Sequence[_]): SchemaDescription = ???
  def buildTableSchemaDescription(table: Table[_]): SchemaDescription = ???

  val emptyHeapDB = HeapBackend.createEmptyDatabase

  trait SimpleQL extends super.SimpleQL with Implicits

  trait Implicits extends super.Implicits {
    implicit def ddlToDDLInvoker(d: SchemaDescription): DDLInvoker = ???
  }

  class QueryExecutorDef[R](tree: Node, param: Any) extends super.QueryExecutorDef[R] {
    def run(implicit session: Backend#Session): R = {
      val inter = new QueryInterpreter(emptyHeapDB, param) {
        override def run(n: Node) = n match {
          case DriverComputation(compiled, driver, _) =>
            val idx = drivers.indexOf(driver)
            if(idx < 0) throw new SlickException("No session found for driver "+driver)
            val driverSession = session.sessions(idx).asInstanceOf[driver.Backend#Session]
            driver.createQueryExecutor[R](compiled, param).run(driverSession)
          case ResultSetMapping(gen, from, CompiledMapping(converter, tpe)) =>
            val fromV = run(from).asInstanceOf[TraversableOnce[Any]]
            val b = n.nodeType.asCollectionType.cons.canBuildFrom()
            b ++= fromV.map(v => converter.read(v.asInstanceOf[QueryInterpreter.ProductValue]))
            b.result()
          case n => super.run(n)
        }
      }
      inter.run(tree).asInstanceOf[R]
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
      val tables = tree.collect { case t: RelationalDriver#Table[_] => t }
      val drivers: Set[RelationalDriver] = tables.map(_.tableProvider)(collection.breakOut)
      if(drivers.size == 1) {
        // Targeting exactly one foreign driver -> Ship the whole thing off
        val compiled = drivers.head.queryCompiler.run(tree).tree
        DriverComputation(compiled, drivers.head, compiled.nodeType)
      } else tree //--
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
