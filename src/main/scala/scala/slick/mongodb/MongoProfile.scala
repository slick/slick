package scala.slick.mongodb

import com.mongodb.casbah.Imports._
import scala.slick.profile.{RelationalMappingCompilerComponent, RelationalDriver, RelationalProfile, Capability}
import scala.slick.ast._
import scala.slick.compiler.{InsertCompiler, CompilerState, CodeGen, QueryCompiler}
import scala.slick.ast.ElementSymbol
import scala.slick.SlickException
import scala.slick.mongodb.MongoProfile.options.MongoCollectionOption
import org.slf4j.LoggerFactory
import scala.slick.util.SlickLogger
import scala.slick.lifted.Query


trait MongoProfile extends RelationalProfile with MongoTypesComponent { driver: MongoDriver =>
  protected[this] lazy val logger = new SlickLogger(LoggerFactory.getLogger(classOf[MongoProfile]))

  type Backend = MongoBackend
  val backend: Backend = MongoBackend

  val simple: SimpleQL = new SimpleQL {}

  val Implicit: Implicits = new Implicits {  }
  trait Implicits extends super.Implicits with ImplicitColumnTypes {
    implicit def ddlToDDLInvoker(d: SchemaDescription): DDLInvoker = d.asInstanceOf[DDLInvoker]
  }

  type ColumnType[T] = MongoType[T]
  type BaseColumnType[T] = MongoType[T] with BaseTypedType[T]
  val columnTypes = new MongoTypes

  type SchemaDescription = SchemaDescriptionDef
  type InsertInvoker[T] = InsertInvokerDef[T]
  type QueryExecutor[R] = QueryExecutorDef[R]

  val compiler = QueryCompiler.standard // todo - do we need a special compiler? do we need relational to do the join with DBRef?
  lazy val queryCompiler = compiler + new MongoCodeGen
  lazy val updateCompiler = compiler
  lazy val deleteCompiler = compiler
  lazy val insertCompiler = QueryCompiler(new MongoInsertCompiler)

  trait SimpleQL extends super.SimpleQL with Implicits

  def createQueryExecutor[R](tree: Node, param: Any): QueryExecutorDef[R] = new QueryExecutorDef[R](tree, param)
  def createInsertInvoker[T](tree: scala.slick.ast.Node): InsertInvoker[T] = new InsertInvokerDef[T](tree)
  def compileParameterizedQuery[P,R](q: Query[_, R]) = ??? // new ParameterizedQuery[P, R](queryCompiler.run(Node(q)).tree)

  // TODO - Update Invoker?
  def buildSequenceSchemaDescription(seq: Sequence[_]): SchemaDescription = ???
  // TODO - this will be important later as we add support for Capped Collections. Otherwise CreateCollection is mostly a NOOP
  def buildTableSchemaDescription(table: Table[_]): SchemaDescription = new TableDDL(table)

  class QueryExecutorDef[R](tree: Node, param: Any) extends super.QueryExecutorDef[R] {
    def run(implicit session: Backend#Session): R = {
      val inter = new QueryInterpreter(session.database, param) {
        override def run(n: Node) = n match {
          case n => super.run(n)
        }
      }
      inter.run(tree).asInstanceOf[R]
    }
  }

  class InsertInvokerDef[T](tree: Node) extends super.InsertInvokerDef[T] {
    protected[this] val ResultSetMapping(_, Insert(_, table: TableNode, projection, _), CompiledMapping(converter, _)) = tree

    // TODO -this needs to basically always/only be a DBObject...
    def +=(value: T)(implicit session: Backend#Session) {
      val tbl = session.database.getTable(table.tableName)
      logger.debug(s"Insert value '$value', table '$table', projection '$projection', converter '$converter'")
      value match {
        case doc: DBObject =>
          tbl.insert(doc)
        case other =>
          throw new SlickException("Don't know how to insert an instance of '" + other.getClass + "' to MongoDB (" + other.toString + ")")
      }
    }

    def ++=(values: Iterable[T])(implicit session: Backend#Session) {
      val tbl = session.database.getTable(table.tableName)
      logger.debug(s"Insert values '$values', table '$table', projection '$projection', converter '$converter'")
      for (value <- values) value match {
        case doc: DBObject =>
          tbl.insert(doc)
        case other =>
          throw new SlickException("Don't know how to insert an instance of '" + other.getClass + "' to MongoDB (" + other.toString + ")")
      }
    }
  }

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




