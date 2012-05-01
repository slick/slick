package scala.slick.driver

import java.sql.PreparedStatement
import scala.slick.session.{PositionedParameters, PositionedResult}
import scala.slick.ql.Query
import scala.slick.jdbc.{UnitInvokerMixin, MutatingStatementInvoker, MutatingUnitInvoker}
import slick.util.RecordLinearizer

class BasicQueryInvoker[Q, R](q: Query[Q, _ <: R], profile: BasicProfile)
  extends MutatingStatementInvoker[Unit, R] with UnitInvokerMixin[R] with MutatingUnitInvoker[R] {

  override protected val delegate = this

  protected lazy val sres = profile.buildSelectStatement(q)

  def selectStatement = getStatement

  protected def getStatement = sres.sql

  protected def setParam(param: Unit, st: PreparedStatement): Unit = sres.setter(new PositionedParameters(st), null)

  protected def extractValue(rs: PositionedResult): R = sres.linearizer.narrowedLinearizer.asInstanceOf[RecordLinearizer[R]].getResult(profile, rs)

  protected def updateRowValues(rs: PositionedResult, value: R) = sres.linearizer.narrowedLinearizer.asInstanceOf[RecordLinearizer[R]].updateResult(profile, rs, value)

  def invoker: this.type = this
}
