package slick.jdbc

import java.sql.{PreparedStatement, ResultSet}

import scala.language.existentials

import slick.ast.{CompiledStatement, Node, ParameterSwitch, ResultSetMapping}
import slick.relational.{CompiledMapping, ResultConverter}
import slick.util.SQLBuilder

trait JdbcInvokerComponent { self: JdbcProfile =>

  def createQueryInvoker[R](tree: Node, param: Any, sql: String): QueryInvokerImpl[R] = new QueryInvokerImpl[R](tree, param, sql)

  // Parameters for invokers -- can be overridden by profiles as needed
  protected val invokerMutateConcurrency: ResultSetConcurrency = ResultSetConcurrency.Updatable
  protected val invokerMutateType: ResultSetType = ResultSetType.Auto
  protected val invokerPreviousAfterDelete = false

  /** An Invoker for queries. */
  trait QueryInvoker[R] extends StatementInvoker[R] {
    def invoker: this.type = this
  }

  class QueryInvokerImpl[R](tree: Node, param: Any, overrideSql: String) extends QueryInvoker[R] {
    protected[this] val ResultSetMapping(_, compiled, CompiledMapping(_converter, _)) = tree: @unchecked
    protected[this] val converter = _converter.asInstanceOf[ResultConverter[ResultSet, PreparedStatement, ResultSet, R]]
    protected[this] val CompiledStatement(_, sres: SQLBuilder.Result, _) = findCompiledStatement(compiled)

    protected[this] def findCompiledStatement(n: Node): CompiledStatement = n match {
      case c: CompiledStatement => c
      case ParameterSwitch(cases, default) =>
        findCompiledStatement(cases.find { case (f, n) => f(param) }.map(_._2).getOrElse(default))
    }

    protected def getStatement = if(overrideSql ne null) overrideSql else sres.sql
    protected def setParam(st: PreparedStatement): Unit = sres.setter(st, 1, param)
    def extractValue(pr: PositionedResult): R = converter.read(pr.rs)
    def updateRowValues(pr: PositionedResult, value: R) = converter.update(value, pr.rs)
  }
}
