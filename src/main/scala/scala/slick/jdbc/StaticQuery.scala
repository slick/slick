package scala.slick.jdbc

import scala.language.implicitConversions
import java.sql.{ PreparedStatement, SQLWarning }
import collection.mutable.ArrayBuffer
import scala.slick.util.CloseableIterator

/**
 * Invoker for raw SQL queries.
 * The companion object contains utility methods for building static queries.
 */
abstract class StaticQuery[-P, +R](query: String, rconv: GetResult[R], pconv: SetParameter[P])
    extends StatementInvoker[P, R] {
  def getStatement = query
  protected def setParam(param: P, st: PreparedStatement) = pconv(param, new PositionedParameters(st))
  protected def extractValue(rs: PositionedResult): R = rconv(rs)

  protected[this]type Self <: StaticQuery[P, R]
  protected[this] def copy(query: String = this.query, pconv: SetParameter[P] = this.pconv): Self

  def +(s: String) = copy(query + s)
  def +?[T](v: T)(implicit p: SetParameter[T]) = copy(query + '?', new SetParameter[P] {
    def apply(param: P, pp: PositionedParameters) {
      pconv(param, pp)
      p(v, pp)
    }
  })
}

object StaticQuery {
  def apply[R](implicit conv: GetResult[R]) = queryNA("")
  def apply[P, R](implicit pconv1: SetParameter[P], rconv: GetResult[R]) = query[P, R]("")
  def u = updateNA("")
  def u1[P](implicit pconv1: SetParameter[P]) = update[P]("")

  def query[P, R](query: String)(implicit rconv: GetResult[R], pconv: SetParameter[P]) =
    new StaticQuery1[P, R](query, rconv, pconv)

  def queryNA[R](query: String)(implicit conv: GetResult[R]) =
    new StaticQuery0[R](query, conv)

  def update[P](query: String)(implicit pconv: SetParameter[P]) =
    new StaticQuery1[P, Int](query, GetResult.GetUpdateValue, pconv)

  def updateNA(query: String) =
    new StaticQuery0[Int](query, GetResult.GetUpdateValue)

  @inline implicit def interpolation(s: StringContext) = new SQLInterpolation(s)
}

class SQLInterpolation(val s: StringContext) extends AnyVal {
  /** Build a SQLInterpolationResult via string interpolation */
  def sql[P](param: P)(implicit pconv: SetParameter[P]) =
    new SQLInterpolationResult[P](s.parts, param, pconv)
  /** Build a StaticQuery for an UPDATE statement via string interpolation */
  def sqlu[P](param: P)(implicit pconv: SetParameter[P]) = sql(param).asUpdate
}

case class SQLInterpolationResult[P](strings: Seq[String], param: P, pconv: SetParameter[P]) {
  def as[R](implicit rconv: GetResult[R]): StaticQuery0[R] = {
    if (strings.length == 1)
      new StaticQuery0[R](strings(0), rconv)
    else {
      val (convs, params) = pconv match {
        case pconv: SetTupleParameter[_] =>
          (pconv.children.iterator, param.asInstanceOf[Product].productIterator)
        case _ => (Iterator(pconv), Iterator(param))
      }
      val b = new StringBuilder
      val remaining = new ArrayBuffer[SetParameter[Unit]]
      convs.zip(params).zip(strings.iterator).foreach { zipped =>
        val p = zipped._1._2
        var literal = false
        def decode(s: String): String =
          if (s.endsWith("##")) decode(s.substring(0, s.length - 2)) + "#"
          else if (s.endsWith("#")) { literal = true; s.substring(0, s.length - 1) }
          else s
        b.append(decode(zipped._2))
        if (literal) b.append(p.toString)
        else {
          b.append('?')
          remaining += zipped._1._1.asInstanceOf[SetParameter[Any]].applied(p)
        }
      }
      b.append(strings.last)
      new StaticQuery0[R](b.toString, rconv, new SetParameter[Unit] {
        def apply(u: Unit, pp: PositionedParameters): Unit =
          remaining.foreach(_.apply(u, pp))
      })
    }
  }
  def asUpdate = as[Int](GetResult.GetUpdateValue)
}

class StaticQuery0[R](query: String, rconv: GetResult[R], pconv: SetParameter[Unit] = SetParameter.SetUnit) extends StaticQuery[Unit, R](query, rconv, pconv) with UnitInvokerMixin[R] with ApplicableStaticQuery[Unit, R] { self =>

  protected[this]type Self = StaticQuery0[R]
  protected[this] def copy(query: String, pconv: SetParameter[Unit]): Self = new StaticQuery0(query, rconv, pconv)

  val statement = query
  def setParam(st: PreparedStatement) = pconv((), new PositionedParameters(st))
}

class StaticQuery1[P1, R](query: String, rconv: GetResult[R], pconv: SetParameter[P1]) extends StaticQuery[P1, R](query, rconv, pconv) { self =>
  protected[this]type Self = StaticQuery1[P1, R]
  protected[this] def copy(query: String, pconv: SetParameter[P1]): Self = new StaticQuery1(query, rconv, pconv)

  /**
   * Apply the parameter for this query, creating a parameterless one.
   */
  def applied(parameter: P1) = new AppliedStaticQuery1[P1, R] {
    val statement = self.query
    val rconv = self.rconv
    val pconv = self.pconv
    val appliedParameter = parameter
  }

}

/**
 * Applied static query with parameter.
 */
trait AppliedStaticQuery1[P1, R] extends ApplicableStaticQuery[P1, R] {
  val statement: String
  val rconv: GetResult[R]
  val pconv: SetParameter[P1]
  val appliedParameter: P1

  def setParam(st: PreparedStatement) =
    pconv(appliedParameter, new PositionedParameters(st))

  def extractValue(rs: PositionedResult): R = rconv(rs)

}

sealed trait ApplicableStaticQuery[P, R] { self =>
  def statement: String
  protected def setParam(st: PreparedStatement): Unit
  protected def extractValue(rs: PositionedResult): R

  def executedQuery(maxRows: Int = 0,
    defaultType: ResultSetType = ResultSetType.ForwardOnly,
    defaultConcurrency: ResultSetConcurrency = ResultSetConcurrency.ReadOnly,
    defaultHoldability: ResultSetHoldability = ResultSetHoldability.Default)(implicit session: JdbcBackend#Session): ExecutedStaticQuery[R] = {
    val st = session.prepareStatement(statement, defaultType, defaultConcurrency, defaultHoldability)
    setParam(st)
    var doClose = true
    val (res, warning): (Either[Int, PositionedResultIterator[R]], Option[SQLWarning]) = try {
      st.setMaxRows(maxRows)
      if (st.execute) {
        val rs = new PositionedResultIterator[R](st.getResultSet, maxRows) {
          def closeUnderlying() = st.close()
          def extractValue() = self.extractValue(this)
        }
        doClose = false
        (Right(rs) -> Option(st.getWarnings))
      } else (Left(st.getUpdateCount) -> Option(st.getWarnings))
    } finally if (doClose) st.close()

    new ExecutedStaticQuery[R](res, warning)
  }
}

/**
 * Invoker for already executed static query,
 * with additionnal parametermess methods.
 *
 * @constructor creates invoker based on results of previously executed query
 * @param results Query results, either update count or result set.
 * @param warning Execution/statement warning
 */
class ExecutedStaticQuery[R](results: Either[Int, PositionedResultIterator[R]], val warning: Option[SQLWarning]) extends Invoker[Unit, R] with UnitInvokerMixin[R] {
  def iteratorTo(param: Unit, maxRows: Int)(implicit session: JdbcBackend#Session): CloseableIterator[R] =
    results.fold(r => new CloseableIterator.Single[R](r.asInstanceOf[R]), identity)

}
