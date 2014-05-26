package scala.slick.mongodb

import scala.slick.profile.BasicExecutorComponent
import scala.slick.compiler.QueryCompiler
import scala.slick.ast.Node
import scala.slick.lifted.{FlatShapeLevel, Shape}

/**
 * User: Dmytro Vynokurov
 * Date: 23.05.14
 * Time: 23:18
 */
trait MongoExecutorComponent extends BasicExecutorComponent{driver: MongoDriver =>
  override type QueryExecutor = this.type

  /** The type of query executors returned by the driver */
  type QueryExecutor[T] <: QueryExecutorDef[T]

  /** The type of query executors returned by the driver */
  type UnshapedQueryExecutor[T] <: UnshapedQueryExecutorDef[T]


  /** Create an executor -- this method should be implemented by drivers as needed */
  override def createQueryExecutor[R](tree: Node, param: Any): QueryExecutor[R] = ???
  override def createUnshapedQueryExecutor[M](value: M): UnshapedQueryExecutor[M] = ???

  /** Base class for `QueryExecutor` implementations */
  trait QueryExecutorDef[R] {
    def run(implicit session: Backend#Session): R
    def executor: this.type = this
  }

  // Work-around for SI-3346
  class UnshapedQueryExecutorDef[M](protected[this] val value: M) {
    @inline final def run[U](implicit shape: Shape[_ <: FlatShapeLevel, M, U, _], session: Backend#Session): U =
      executor[U].run

    @inline final def executor[U](implicit shape: Shape[_ <: FlatShapeLevel, M, U, _]): QueryExecutor[U] =
      createQueryExecutor[U](queryCompiler.run(shape.toNode(value)).tree, ())
  }
}
