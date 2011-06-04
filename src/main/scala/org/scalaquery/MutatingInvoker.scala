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

  override def apply(parameter: P): MutatingUnitInvoker[R] = new AppliedInvoker[P,R] with MutatingUnitInvoker[R] {
    protected val appliedParameter = parameter
    protected val delegate = self
  }
}

trait MutatingUnitInvoker[R] extends UnitInvoker[R] {
  override protected val delegate: MutatingInvoker[Param, R]

  def mutate(f: ResultSetMutator[R] => Unit, end: ResultSetMutator[R] => Unit)(implicit session: Session): Unit =
    delegate.mutate(appliedParameter, f, end)(session)

  final def mutate(f: ResultSetMutator[R] => Unit)(implicit session: Session): Unit = mutate(f, null)(session)
}

trait MutatingStatementInvoker[-P,R] extends StatementInvoker[P,R] with MutatingInvoker[P,R] {

  protected def updateRowValues(rs: PositionedResult, value: R)
  protected val mutateConcurrency: ResultSetConcurrency = ResultSetConcurrency.Updatable
  protected val mutateType: ResultSetType = ResultSetType.Auto
  protected val previousAfterDelete = false

  def mutate(param: P, f: ResultSetMutator[R] => Unit, end: ResultSetMutator[R] => Unit)(implicit session: Session): Unit =
    session.withTransaction {
      /* Hsqldb forces ResultSets to be read-only in auto-commit mode, so we
       * use an explicit transaction. It shouldn't hurt other databases. */
      results(param, 0, defaultConcurrency = mutateConcurrency, defaultType = mutateType).fold(
        _ => throw new SQueryException("Cannot transform an update result"),
        pr => try {
          val rs = pr.rs
          var current: R = null.asInstanceOf[R]
          val mu = new ResultSetMutator[R] {
            def row = current
            def row_=(value: R) {
              pr.restart
              updateRowValues(pr, value)
              rs.updateRow()
            }
            def insert(value: R) {
              rs.moveToInsertRow()
              pr.restart
              updateRowValues(pr, value)
              rs.insertRow()
              rs.moveToCurrentRow()
            }
            def delete() {
              rs.deleteRow()
              if(previousAfterDelete) rs.previous()
            }
          }
          while(pr.nextRow) {
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
                pr.restart
                updateRowValues(pr, value)
                rs.insertRow()
              }
            })
          }
        } finally { pr.close() }
      )
    }
}
