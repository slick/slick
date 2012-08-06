package scala.slick.driver

import java.sql.PreparedStatement
import scala.slick.jdbc.MutatingStatementInvoker
import scala.slick.lifted.Query
import scala.slick.session.{PositionedParameters, PositionedResult}
import scala.slick.util.RecordLinearizer

class BasicQueryTemplate[P, R](query: Query[_, R], profile: BasicProfile) extends MutatingStatementInvoker[P, R] {

  protected lazy val sres = profile.buildSelectStatement(query)

  def selectStatement = getStatement

  protected def getStatement = sres.sql

  protected def setParam(param: P, st: PreparedStatement): Unit = sres.setter(new PositionedParameters(st), param)

  protected def extractValue(rs: PositionedResult): R = sres.linearizer.narrowedLinearizer.asInstanceOf[RecordLinearizer[R]].getResult(profile, rs)

  protected def updateRowValues(rs: PositionedResult, value: R) = sres.linearizer.narrowedLinearizer.asInstanceOf[RecordLinearizer[R]].updateResult(profile, rs, value)
}
