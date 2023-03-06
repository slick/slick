package slick.jdbc

import java.sql.PreparedStatement

import scala.collection.mutable.ArrayBuffer

import slick.dbio.Effect
import slick.sql.SqlStreamingAction

class ActionBasedSQLInterpolation(val s: StringContext) extends AnyVal {
  /** Build a SQLActionBuilder via string interpolation */
  def sql[P](param: P = ())(implicit pconv: SetParameter[P]): SQLActionBuilder[P] =
    new SQLActionBuilder[P](s.parts, param, pconv)
  /** Build an Action for an UPDATE statement via string interpolation */
  def sqlu[P](param: P = ())(implicit pconv: SetParameter[P]) = sql(param).asUpdate
}

object SQLInterpolation {
  def parse[P](strings: Seq[String], param: P, pconv: SetParameter[P]): (String, SetParameter[Unit]) = {
    if(strings.length == 1) (strings(0), SetParameter.SetUnit)
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
      (b.toString, new SetParameter[Unit] {
        def apply(u: Unit, pp: PositionedParameters): Unit =
          remaining.foreach(_.apply(u, pp))
      })
    }
  }
}

case class SQLActionBuilder[P](strings: Seq[String], param: P, pconv: SetParameter[P]) {
  def as[R](implicit rconv: GetResult[R]): SqlStreamingAction[Vector[R], R, Effect] = {
    val (sql, unitPConv) = SQLInterpolation.parse(strings, param, pconv)
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
