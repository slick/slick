package slick.jdbc

import java.sql.PreparedStatement

import scala.collection.mutable.ArrayBuffer
import scala.language.implicitConversions

import slick.dbio.Effect
import slick.sql.SqlStreamingAction

class ActionBasedSQLInterpolation(val s: StringContext) extends AnyVal {
  /** Build a SQLActionBuilder via string interpolation */
  def sql(params: TypedParameter[_]*): SQLActionBuilder =
    new SQLActionBuilder(s.parts, params)

  /** Build an Action for an UPDATE statement via string interpolation */
  def sqlu(params: TypedParameter[_]*) = sql(params: _*).asUpdate
}

class TypedParameter[T](val param: T, val pconv: SetParameter[T])

object TypedParameter {
  implicit def typedParameter[T](param: T)(implicit pconv: SetParameter[T]): TypedParameter[T] =
    new TypedParameter[T](param, pconv)
}

object SQLInterpolation {
  def parse(strings: Seq[String], tparams: Seq[TypedParameter[Any]]): (String, SetParameter[Unit]) = {
    if(strings.length == 1) (strings(0), SetParameter.SetUnit)
    else {
      val b = new StringBuilder
      val remaining = new ArrayBuffer[SetParameter[Unit]]
      tparams.zip(strings.iterator).foreach { zipped =>
        val p = zipped._1.param
        var literal = false
        def decode(s: String): String =
          if(s.endsWith("##")) decode(s.substring(0, s.length-2)) + "#"
          else if(s.endsWith("#")) { literal = true; s.substring(0, s.length-1) }
          else s
        b.append(decode(zipped._2))
        if(literal) b.append(p.toString)
        else {
          b.append('?')
          remaining += zipped._1.pconv.applied(p)
        }
      }
      b.append(strings.last)
      (b.toString, new SetParameter[Unit] {
        def apply(u: Unit, pp: PositionedParameters): Unit =
          remaining.foreach(_.apply(u, pp))
      })
    }
  }
}

case class SQLActionBuilder(strings: Seq[String], params: Seq[TypedParameter[_]]) {
  def as[R](implicit rconv: GetResult[R]): SqlStreamingAction[Vector[R], R, Effect] = {
    val (sql, unitPConv) = SQLInterpolation.parse(strings, params.asInstanceOf[Seq[TypedParameter[Any]]])
    new StreamingInvokerAction[Vector[R], R, Effect] {
      def statements = List(sql)
      protected[this] def createInvoker(statements: Iterable[String]) = new StatementInvoker[R] {
        val getStatement = statements.head
        protected def setParam(st: PreparedStatement) = unitPConv((), new PositionedParameters(st))
        protected def extractValue(rs: PositionedResult): R = rconv(rs)
      }
      protected[this] def createBuilder = Vector.newBuilder[R]
    }
  }
  def asUpdate = as[Int](GetResult.GetUpdateValue).head
}
