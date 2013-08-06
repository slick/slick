package scala.slick.mongodb

import scala.slick.ast._
import scala.slick.ast.Util._
import scala.slick.ast.TypeUtil._
import scala.slick.util.{SlickLogger, SQLBuilder}
import scala.slick.profile._
import scala.slick.backend.DatabaseComponent
import scala.slick.jdbc.JdbcBackend
import org.slf4j.LoggerFactory

import com.mongodb.casbah.Imports._
import com.mongodb.casbah.MongoClientURI
import com.mongodb.CommandResult









trait MongoExecutorComponent extends BasicExecutorComponent { driver: MongoDriver =>

  type QueryExecutor[T] <: QueryExecutorDef[T]

  def createQueryExecutor[R](tree: Node, param: Any): QueryExecutor[R] = new MongoQueryExecutorDef[R](tree, param)

  class MongoQueryExecutorDef[R](tree: Node, param: Any) extends super.QueryExecutorDef[R] {

    // TODO - Make me something Mongo-ey
    lazy val selectStatement =
      tree.findNode(_.isInstanceOf[CompiledStatement]).get
        .asInstanceOf[CompiledStatement].extra.asInstanceOf[SQLBuilder.Result].sql // TODO - Obviously we don't want a bleeding SQLBuilder

    def run(implicit session: Backend#Session): R = { tree match {
      // see JdbcExecutorComponent L:21
      case rsm: ResultSetMapping =>
        createMongoQueryInvoker[Any, Any](rsm).to(param)(session, rsm.nodeType.asCollectionType.cons.canBuildFrom)
      case First(rsm: ResultSetMapping) =>
        createMongoQueryInvoker[Any, Any](rsm).first(param)
    }}.asInstanceOf[R]
  }
}

trait MongoInvokerComponent extends BasicInvokerComponent { driver: MongoDriver =>
  type InsertInvoker[T] = MongoInsertInvoker[T]

  def createInsertInvoker[U](tree: Node) = createMongoInsertInvoker(tree)

  // Create the different invokers –– these methods should be overriden by drivers as needed
  def createMongoInsertInvoker[I](tree: Node) = new MongoInsertInvoker[I](tree)
  def createMongoCommandInvoker[C](tree: Node) = new MongoCommandInvoker[C](tree)
  def createMongoUpdateInvoker[U](tree: Node) = new MongoUpdateInvoker[U](tree)
  def createMongoDeleteInvoker[D](tree: Node) = new MongoDeleteInvoker[D](tree)
  def createMongoQueryInvoker[P, R](tree: Node): MongoQueryInvoker[P, R] = new MongoQueryInvoker[P, R](tree)

  // Parameters for invokers -- can be overriden blah blah blah
  // TODO - see JdbcInvokerComponent L: 26 - we may want to model similarly

  /** A parameterized query invoker */
  class MongoQueryInvoker[P, R](protected val tree: Node) extends MongoInvoker[P, R] {
    protected[this] val ResultSetMapping(_ , _ , _ /* TODO - FILL ME IN
      CompiledStatement(_, sres: SQLBuilder.Result),
      CompiledMapping() */) = tree

    def queryObject: DBObject = ???
    def invoker: this.type = this
  }

  /** Invoker for executing commands, which return Unit */
  class MongoCommandInvoker[C](tree: Node) extends MongoQueryInvoker[Unit, C](tree) {
    def commandObject: DBObject = ???
  }

  /** Pseudo-invoker for running Deletions */
  class MongoDeleteInvoker(protected val tree: Node) {
    protected[this] val ResultSetMapping(_, _, _) = tree

    def deleteObject: DBObject = ???
    def delete(implicit session: Backend#Session): WriteResult = ???

    def deleteInvoker: this.type = this
  }

  /** Pseudo-Invoker for insertions
    * TODO - Properly support UPSERT
    */
  class MongoInsertInvoker[I](tree: Node) {

    protected[this] val ResultSetMapping(_, insertNode: Insert, _ /*CompiledMapping(converter, _) */) = tree
    protected[this] lazy val builder = createInsertBuilder(insertNode)

    protected lazy val insertResult = builder.buildInsert
    lazy val insertObject: DBObject = insertResult.toDBObject

    // TODO - insertStatementFor Shape???

    def insert(value: I)(implicit session: Backend#Session): Unit = ???

    /** Insert multiple rows. Uses the Mongo driver batch feature.
      * Right now, no return - just exceptions thrown if DB tosses an error.
      */
    def insertAll(values: I*)(implicit session: Backend#Session): Unit = ???

    /** Insert a single value */
    def +=(value: I)(implicit session: Backend#Session): Unit = insert(value)

    /** Insert a collection of values */
    def ++=(values: Iterable[I])(implicit session: Backend#Session): Unit = insertAll(values.toSeq: _*)

    def insertInvoker: this.type = this
  }

  /** Pseudo-Invoker for running Updates.
    * TODO - Upsert support ?
    *
    */
  class MongoUpdateInvoker[U](protected val tree: Node) {
    protected[this] val ResultSetMapping(_, _, _) = tree // TODO - fill me out

    def updateObject: DBObject = ???

    def update(value: U)(implicit session: Backend#Session): Unit = ???

    def updateInvoker: this.type = this
  }
}

/** Base trait for Mongo Statement invokers, using parameter type P & Result type R */
trait MongoInvoker[-P, +R] extends Function1[P, MongoUnitInvoker[R]] { self =>

  /**
   * Execute the statement & return a CloseableIterator of the converted results.
   * The iterator must be either fully ready or closed explicitly (In MongoDB's case the iterator *should*
   * get automatically cleaned up)
   */


}

trait MongoUnitInvoker[+R]

trait MongoTableComponent extends RelationalTableComponent { driver: MongoDriver =>

  trait ColumnOptions extends super.ColumnOptions {
    def DBType(dbType: String) = ColumnOption.DBType(dbType)
  }

  override val columnOptions: ColumnOptions = new AnyRef with ColumnOptions
}