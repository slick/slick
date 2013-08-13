package scala.slick.mongodb

import com.mongodb.casbah.Imports._
import scala.slick.profile.{RelationalMappingCompilerComponent, RelationalDriver, RelationalProfile, Capability}
import scala.slick.ast._
import scala.slick.compiler.{CompilerState, CodeGen, QueryCompiler}
import scala.slick.ast.ElementSymbol
import scala.slick.SlickException
import scala.slick.mongodb.MongoProfile.options.MongoCollectionOption


trait MongoProfile extends RelationalProfile { driver: MongoDriver =>
  type Backend = MongoBackend
  val backend: Backend = MongoBackend

  val Implicit: Implicits = new Implicits {  }
  trait Implicits extends LowPriorityImplicits with super.Implicits with ImplicitColumnTypes {
    implicit def ddlToDDLInvoker(d: SchemaDescription): DDLInvoker = d.asInstanceOf[DDLInvoker]
  }

  type ColumnType[T] = MongoType[T]
  type BaseColumnType[T] = MongoType[T] with BaseTypedType[T]
  val columnTypes = new MongoTypes

  val compiler = QueryCompiler.standard // todo - do we need a special compiler? do we need relational to do the join with DBRef?
  lazy val queryCompiler = compiler + new MongoCodeGen
  lazy val updateCompiler = compiler
  lazy val deleteCompiler = compiler
  lazy val insertCompiler = QueryCompiler(new MongoInsertCompiler)
  type ColumnType[T] = MongoType[T]
  type BaseColumnType[T] = MongoType[T] with BaseTypedType[T]

  def createQueryExecutor[R](tree: Node, param: Any): QueryExecutorDef[R] = new QueryExecutorDef[R](tree, param)
  def createInsertInvoker[T](tree: scala.slick.ast.Node): InsertInvoker[T] = new InsertInvokerDef[T](tree)
  // TODO - Update Invoker?
  def buildSequenceSchemaDescription(seq: Sequence[_]): SchemaDescription = ???
  // TODO - this will be important later as we add support for Capped Collections. Otherwise CreateCollection is mostly a NOOP
  def buildTableSchemaDescription(table: Table[_]): SchemaDescription = new TableDDL(table)

  abstract class DDL extends SchemaDescriptionDef with DDLInvoker { self =>
    // todo - should we really support ++ In mongo DDLs?
    def ++(other: SchemaDescription): SchemaDescription = {
      val d = Implicit.ddlToDDLInvoker(other)
      new DDL {
        def create(implicit session: Backend#Session) { self.create; d.create }
        def drop(implicit session: Backend#Session) { self.drop; d.drop }
      }
    }
  }

  class TableDDL(table: Table[_]) extends DDL {
    def create(implicit session: Backend#Session): Unit = {
      session.database.createTable(table.tableName,
        Seq.empty[MongoCollectionOption] /** TODO - Figure out how ot extract Collection Options! */,
        table.indexes.toIndexedSeq, table.tableConstraints.toIndexedSeq)
    }
    def drop(implicit session: Backend#Session): Unit =
      session.database.dropTable(table.tableName)
  }
}

object MongoProfile {
  object capabilities {
    /** Supports aggregation framework */
    val aggregation = Capability("mongodb.aggregation")
    /** Supports the V8 JavaScript engine */
    val v8JSEngine = Capability("mongodb.jsengine.v8")
    /** Supports the SpiderMonkey JavaScript engine  (v8 is better and allows more features ) */
    val spiderMonkeyJSEngine = Capability("mongodb.jsengine.spidermonkey")

    /** Supports all MongoDB features which do not have separate capability values */
    val other = Capability("mongodb.other")

    /** All MongoDB capabilities */
    val all = Set(aggregation, v8JSEngine, spiderMonkeyJSEngine, other)
  }




  object options {
    sealed trait MongoCollectionOption
    case object CollectionCapped extends MongoCollectionOption
    case class CollectionAutoIndexID(value: Boolean = true) extends MongoCollectionOption
    case class CollectionSizeBytes(value: Long) extends MongoCollectionOption
    case class CollectionMaxDocuments(value: Long) extends MongoCollectionOption
    /**
     * Optional. Enables a capped collection. To create a capped collection, specify true.
     * If you specify true, you must also set a maximum size in the size field.
     *
     * @see http://docs.mongodb.org/manual/reference/glossary/#term-capped-collection
     */
    val capped = CollectionCapped
    /**
     * Optional. If capped is true, specify false to disable the automatic creation of an index on the _id field.
     * Before 2.2, the default value for autoIndexId was false.
     *
     * @see http://docs.mongodb.org/manual/release-notes/2.2/#id-indexes-capped-collections
     */
    def noAutoIndexID = CollectionAutoIndexID(false)
    def autoIndexID = CollectionAutoIndexID(true)

    /**
     * Optional. Specifies a maximum size in bytes for a capped collection. The size field is required for capped collections.
     * If capped is false, you can use this field to preallocate space for an ordinary collection.
     */
    def sizeBytes(value: Long) = CollectionSizeBytes(value)

    /**
     * Optional. The maximum number of documents allowed in the capped collection.
     * The sizeBytes limit takes precedence over this limit.
     * If a capped collection reaches its maximum size before it reaches the maximum number of documents,
     * MongoDB removes old documents.
     * If you prefer to use this limit, ensure that the size limit, which is required, is sufficient to contain the documents limit.
     */
    def maxDocuments(value: Long) = CollectionMaxDocuments(value)
  }



}



/**
 * Slick driver for MongoDB
 *
 * Based on Casbah, Fully synchronous. A rough sketch of the ultimate plan for a full fledged
 * MongoDB mapping
 *
 * @author bwmcadams
 */
class MongoDriver extends RelationalDriver
                  with MongoProfile
                  with MongoTypesComponent
                  with RelationalMappingCompilerComponent { driver =>

  //type RowReader = QueryInterpreter.ProductValue

  // TODO - Detect Mongo JS Engine and access to aggregation at connection time
  override protected def computeCapabilities: Set[Capability] = (super.computeCapabilities
    - RelationalProfile.capabilities.foreignKeyActions /** we might be able to gerryMander this around DBRefs, later */
    - RelationalProfile.capabilities.functionDatabase /** I think we need to be able to do this in an exact SQL Way...? */
    - RelationalProfile.capabilities.functionUser
    - RelationalProfile.capabilities.joinFull
    - RelationalProfile.capabilities.joinRight
    - RelationalProfile.capabilities.likeEscape
    - RelationalProfile.capabilities.pagingDrop
    - RelationalProfile.capabilities.pagingNested
    - RelationalProfile.capabilities.pagingPreciseTake
    - RelationalProfile.capabilities.typeBigDecimal /** MongoDB has no safe type mapping for BigDecimal */
    - RelationalProfile.capabilities.zip /** TODO - talk to Stefan about what zip capabilities means/need */
  )

  trait QueryMappingCompiler extends super.MappingCompiler {
    def createColumnConverter(n: Node, path: Node, option: Boolean): ResultConverter = { // todo - fill me in
      val Select(_, ElementSymbol(ridx)) = path
      val nullable = typeInfoFor(n.nodeType).nullable
      new ResultConverter {
        def read(pr: RowReader) = {
          val v = pr(ridx-1)
          if (!nullable && (v.asInstanceOf[AnyRef] eq null)) throw new SlickException("Read null value for non-nullable column")
          v
        }
        def update(value: Any, pr: RowUpdater) = ???
        def set(value: Any, pp: RowWriter) = ???
      }
    }
  }

  class MongoCodeGen extends CodeGen with QueryMappingCompiler {
    def apply(state: CompilerState): CompilerState = state.map(n => retype(apply(n, state)))

    def apply(node: Node, state: CompilerState): Node = // todo - evaluate. Should still be CSO?
      ClientSideOp.mapResultSetMapping(node, keepType = true) { rsm =>
        val nmap = CompiledMapping(compileMapping(rsm.map), rsm.map.nodeType)
        rsm.copy(map = nmap).nodeTyped(rsm.nodeType)
      }

    def retype(n: Node): Node = {
      val n2 = n.nodeMapChildrenKeepType(retype)
      n2.nodeRebuildWithType(trType(n2.nodeType))
    }

    def trType(t: Type): Type = t match {
      case StructType(el) => StructType(el.map { case (s, t) => (s, trType(t)) })
      case ProductType(el) => ProductType(el.map(trType))
      case CollectionType(cons, el) => CollectionType(cons, trType(el))
      case t => typeInfoFor(t)
    }


  }


}
