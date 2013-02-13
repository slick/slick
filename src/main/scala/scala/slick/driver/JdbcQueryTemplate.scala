package scala.slick.driver

import scala.slick.ast.Node
import scala.slick.compiler.CodeGen
import scala.slick.jdbc.{MutatingStatementInvoker, PositionedParameters, PositionedResult}
import scala.slick.util.{SQLBuilder, ValueLinearizer, RecordLinearizer}
import java.sql.PreparedStatement

class JdbcQueryTemplate[P, R](tree: Node, linearizer: ValueLinearizer[_], driver: JdbcDriver) extends MutatingStatementInvoker[P, R] {

  protected val (_, sres: SQLBuilder.Result) = CodeGen.findResult(tree)

  def selectStatement = getStatement

  protected def getStatement = sres.sql

  protected def setParam(param: P, st: PreparedStatement): Unit = sres.setter(new PositionedParameters(st), param)

  protected def extractValue(rs: PositionedResult): R = linearizer.narrowedLinearizer.asInstanceOf[RecordLinearizer[R]].getResult(driver, rs)

  protected def updateRowValues(rs: PositionedResult, value: R) = linearizer.narrowedLinearizer.asInstanceOf[RecordLinearizer[R]].updateResult(driver, rs, value)
}
