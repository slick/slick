package org.scalaquery

import org.scalaquery.session._

trait MutatingInvoker[-P,R] extends Invoker[P,R] { self =>
  /**
   * Transform a query's results with an updatable result set.
   */
  def mutate(param: P, f: ResultSetMutator[R] => Unit, end: ResultSetMutator[R] => Unit)(implicit session: Session): Unit

  /**
   * Transform a query's results with an updatable result set.
   */
  final def mutate(param: P)(f: ResultSetMutator[R] => Unit)(implicit session: Session): Unit = mutate(param, f, null)(session)

  override def apply(parameter: P): MutatingUnitInvoker[R] = new AppliedInvoker[P,R] with DelegatingMutatingUnitInvoker[P,R] {
    protected val appliedParameter = parameter
    protected val delegate = self
  }
}

trait MutatingUnitInvoker[R] extends UnitInvoker[R] {
  def mutate(f: ResultSetMutator[R] => Unit, end: ResultSetMutator[R] => Unit)(implicit session: Session): Unit
  final def mutate(f: ResultSetMutator[R] => Unit)(implicit session: Session): Unit = mutate(f, null)(session)
}

trait DelegatingMutatingUnitInvoker[P,R] extends DelegatingUnitInvoker[P, R] with MutatingUnitInvoker[R] {
  override protected val delegate: MutatingInvoker[P, R]

  def mutate(f: ResultSetMutator[R] => Unit, end: ResultSetMutator[R] => Unit)(implicit session: Session): Unit =
    delegate.mutate(appliedParameter, f, end)(session)
}

trait MutatingStatementInvoker[-P,R] extends StatementInvoker[P,R] with MutatingInvoker[P,R] {

  protected def updateRowValues(rs: PositionedResult, value: R)

  def mutate(param: P, f: ResultSetMutator[R] => Unit, end: ResultSetMutator[R] => Unit)(implicit session: Session): Unit =
    session.withTransaction {
      /* Hsqldb forces ResultSets to be read-only in auto-commit mode, so we
       * use an explicit transaction. It shouldn't hurt other databases. */
      results(param, 0, defaultConcurrency = ResultSetConcurrency.Updatable) match {
        case Left(_) => throw new SQueryException("Cannot transform an update result")
        case Right(pr) => try {
          val rs = pr.rs
          var current: R = null.asInstanceOf[R]
          val mu = new ResultSetMutator[R] {
            def row = current
            def row_=(value: R) {
              pr.pos = 0
              updateRowValues(pr, value)
              rs.updateRow()
            }
            def insert(value: R) {
              rs.moveToInsertRow()
              pr.pos = 0
              updateRowValues(pr, value)
              rs.insertRow()
              rs.moveToCurrentRow()
            }
            def delete() { rs.deleteRow() }
          }
          while(pr.next) {
            current = extractValue(pr)
            f(mu)
          }
          if(end ne null) {
            end(new ResultSetMutator[R] {
              def row = throw new SQueryException("After end of result set")
              def row_=(value: R) = throw new SQueryException("After end of result set")
              def delete() = throw new SQueryException("After end of result set")
              def insert(value: R) {
                rs.moveToInsertRow()
                pr.pos = 0
                updateRowValues(pr, value)
                rs.insertRow()
              }
            })
          }
        } finally { pr.close() }
      }
    }
}
