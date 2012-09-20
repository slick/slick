package scala.slick.jdbc

import scala.language.implicitConversions
import java.sql.PreparedStatement
import collection.mutable.ArrayBuffer

/**
 * Invoker for raw SQL queries.
 * The companion object contains utility methods for building static queries.
 */
abstract class StaticQuery[-P,+R](query: String, rconv: GetResult[R], pconv: SetParameter[P])
extends StatementInvoker[P,R] {
  def getStatement = query
  protected def setParam(param: P, st: PreparedStatement) = pconv(param, new PositionedParameters(st))
  protected def extractValue(rs: PositionedResult): R = rconv(rs)

  protected[this] type Self <: StaticQuery[P, R]
  protected[this] def copy(query: String = this.query, pconv: SetParameter[P] = this.pconv): Self

  def + (s: String) = copy(query + s)
  def +? [T](v: T)(implicit p: SetParameter[T]) = copy(query + '?', new SetParameter[P] {
    def apply(param: P, pp: PositionedParameters) {
      pconv(param, pp)
      p(v, pp)
    }
  })
}

object StaticQuery {
  def apply[R](implicit conv: GetResult[R]) = queryNA("")
  def apply[P, R](implicit pconv1: SetParameter[P],  rconv: GetResult[R]) = query[P,R]("")
  def u = updateNA("")
  def u1[P](implicit pconv1: SetParameter[P]) = update[P]("")

  def query[P,R](query: String)(implicit rconv: GetResult[R], pconv: SetParameter[P]) =
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
    if(strings.length == 1)
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
          if(s.endsWith("##")) decode(s.substring(0, s.length-2)) + "#"
          else if(s.endsWith("#")) { literal = true; s.substring(0, s.length-1) }
          else s
        b.append(decode(zipped._2))
        if(literal) b.append(p.toString)
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

class StaticQuery0[R](query: String, rconv: GetResult[R], pconv: SetParameter[Unit] = SetParameter.SetUnit) extends StaticQuery[Unit, R](query, rconv, pconv) with UnitInvokerMixin[R] {
  protected[this] type Self = StaticQuery0[R]
  protected[this] def copy(query: String, pconv: SetParameter[Unit]): Self = new StaticQuery0(query, rconv, pconv)
}

class StaticQuery1[P1, R](query: String, rconv: GetResult[R], pconv: SetParameter[P1]) extends StaticQuery[P1, R](query, rconv, pconv) {
  protected[this] type Self = StaticQuery1[P1, R]
  protected[this] def copy(query: String, pconv: SetParameter[P1]): Self = new StaticQuery1(query, rconv, pconv)
}
