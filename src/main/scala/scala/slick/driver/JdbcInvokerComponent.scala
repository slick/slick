package scala.slick.driver

import scala.language.{higherKinds, existentials}
import java.sql.PreparedStatement
import scala.slick.ast.{CompiledStatement, ResultSetMapping, Node, ParameterSwitch}
import scala.slick.jdbc._
import scala.slick.util.SQLBuilder
import scala.slick.profile.BasicInvokerComponent
import scala.slick.relational.{ResultConverter, CompiledMapping}

trait JdbcInvokerComponent extends BasicInvokerComponent{ driver: JdbcDriver =>

  // Create the different invokers -- these methods should be overridden by drivers as needed
  def createUpdateInvoker[T](tree: Node, param: Any) = new UpdateInvoker[T](tree, param)
  def createDeleteInvoker(tree: Node, param: Any) = new DeleteInvoker(tree, param)
  def createQueryInvoker[R](tree: Node, param: Any): QueryInvoker[R] = new QueryInvoker[R](tree, param)
  def createDDLInvoker(ddl: SchemaDescription) = new DDLInvoker(ddl)

  // Parameters for invokers -- can be overridden by drivers as needed
  protected val invokerMutateConcurrency: ResultSetConcurrency = ResultSetConcurrency.Updatable
  protected val invokerMutateType: ResultSetType = ResultSetType.Auto
  protected val invokerPreviousAfterDelete = false

  /** An Invoker for queries. */
  abstract class AbstractQueryInvoker[R](tree: Node, param: Any) extends MutatingStatementInvoker[R] {
    override protected val mutateConcurrency = invokerMutateConcurrency
    override protected val mutateType = invokerMutateType
    override protected val previousAfterDelete = invokerPreviousAfterDelete

    protected[this] val ResultSetMapping(_, compiled, CompiledMapping(_converter, _)) = tree
    protected[this] val converter = _converter.asInstanceOf[ResultConverter[JdbcResultConverterDomain, R]]
    protected[this] val CompiledStatement(_, sres: SQLBuilder.Result, _) = findCompiledStatement(compiled)

    protected[this] def findCompiledStatement(n: Node): CompiledStatement = n match {
      case c: CompiledStatement => c
      case ParameterSwitch(cases, default) =>
        findCompiledStatement(cases.find { case (f, n) => f(param) }.map(_._2).getOrElse(default))
    }

    protected def getStatement = sres.sql
    protected def setParam(st: PreparedStatement): Unit = sres.setter(st, 1, param)
    protected def extractValue(pr: PositionedResult): R = converter.read(pr.rs)
    protected def updateRowValues(pr: PositionedResult, value: R) = converter.update(value, pr.rs)
    def invoker: this.type = this
  }

  class QueryInvoker[R](tree: Node, param: Any) extends AbstractQueryInvoker[R](tree,param){
    def forUpdate = new ForUpdateQueryInvoker(tree, param)
  }

  class ForUpdateQueryInvoker[R](tree: Node, param: Any) extends AbstractQueryInvoker[R](tree,param){
    override def getStatement = super.getStatement+" for update"
  }

  class DDLInvoker(ddl: DDL) extends super.DDLInvoker {
    def create(implicit session: Backend#Session): Unit = session.withTransaction {
      for(s <- ddl.createStatements)
        session.withPreparedStatement(s)(_.execute)
    }

    def drop(implicit session: Backend#Session): Unit = session.withTransaction {
      for(s <- ddl.dropStatements)
        session.withPreparedStatement(s)(_.execute)
    }
  }

  /** Pseudo-invoker for running DELETE calls. */
  class DeleteInvoker(protected val tree: Node, param: Any) {
    protected[this] val ResultSetMapping(_, CompiledStatement(_, sres: SQLBuilder.Result, _), _) = tree

    def deleteStatement = sres.sql

    def delete(implicit session: Backend#Session): Int = session.withPreparedStatement(deleteStatement) { st =>
      sres.setter(st, 1, param)
      st.executeUpdate
    }

    def deleteInvoker: this.type = this
  }

  /** Pseudo-invoker for running UPDATE calls. */
  class UpdateInvoker[T](protected val tree: Node, param: Any) {
    protected[this] val ResultSetMapping(_,
      CompiledStatement(_, sres: SQLBuilder.Result, _),
      CompiledMapping(_converter, _)) = tree
    protected[this] val converter = _converter.asInstanceOf[ResultConverter[JdbcResultConverterDomain, T]]

    def updateStatement = getStatement

    protected def getStatement = sres.sql

    def update(value: T)(implicit session: Backend#Session): Int = session.withPreparedStatement(updateStatement) { st =>
      st.clearParameters
      converter.set(value, st)
      sres.setter(st, converter.width+1, param)
      st.executeUpdate
    }

    def updateInvoker: this.type = this
  }
}
