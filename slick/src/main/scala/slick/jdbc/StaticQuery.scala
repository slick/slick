package slick.jdbc

import java.sql.PreparedStatement

import scala.collection.immutable.ListMap
import scala.collection.mutable.ArrayBuffer
import scala.language.implicitConversions

import slick.compat.collection.*
import slick.dbio.Effect
import slick.sql.SqlStreamingAction

class ActionBasedSQLInterpolation(val s: StringContext) extends AnyVal {
  /** Build a SQLActionBuilder via string interpolation */
  def sql(params: TypedParameter[?]*): SQLActionBuilder =
    SQLActionBuilder.parse(s.parts, params)

  /** Build an Action for an UPDATE statement via string interpolation */
  def sqlu(params: TypedParameter[?]*) = sql(params *).asUpdate
}

class TypedParameter[T](val param: T, val setParameter: SetParameter[T]) {
  def applied = setParameter.applied(param)
}

object TypedParameter {
  implicit def typedParameter[T](param: T)(implicit setParameter: SetParameter[T]): TypedParameter[T] =
    new TypedParameter[T](param, setParameter)
}

object SQLActionBuilder {
  private val seqOfAny: GetResult[Seq[Any]] = GetResult { pr =>
    val b = Vector.newBuilder[Any]
    while(pr.hasMoreColumns) b += pr.nextObject()
    b.result()
  }

  private val mapOfAny: GetResult[Map[String, Any]] = GetResult { pr =>
    val md = pr.rs.getMetaData
    var m = ListMap.empty[String, Any]
    while(pr.hasMoreColumns) {
      val label = md.getColumnLabel(pr.currentPos + 1)
      val v = pr.nextObject()
      if(!m.contains(label)) m = m.updated(label, v)
    }
    m
  }

  def parse(strings: Seq[String], typedParams: Seq[TypedParameter[?]]): SQLActionBuilder = {
    if (strings.length == 1)
      SQLActionBuilder(strings.head, SetParameter.SetUnit)
    else {
      val b         = new StringBuilder
      val remaining = new ArrayBuffer[SetParameter[Unit]]
      typedParams.zip(strings.iterator.to(Iterable)).foreach { zipped =>
        val p       = zipped._1.param
        var literal = false
        def decode(s: String): String =
          if (s.endsWith("##")) decode(s.substring(0, s.length - 2)) + "#"
          else if (s.endsWith("#")) {
            literal = true
            s.substring(0, s.length - 1)
          }
          else
            s
        b.append(decode(zipped._2))
        if (literal) b.append(p.toString)
        else {
          b.append('?')
          remaining += zipped._1.applied
        }
      }
      b.append(strings.last)
      SQLActionBuilder(b.toString, (u, pp) => remaining.foreach(_(u, pp)))
    }
  }
}

case class SQLActionBuilder(sql: String, setParameter: SetParameter[Unit]) {
  def as[R](implicit getResult: GetResult[R]): SqlStreamingAction[Vector[R], R, Effect] = {
    new StreamingInvokerAction[Vector[R], R, Effect] {
      def statements: Iterable[String] = List(sql)
      protected[this] def createInvoker(statements: Iterable[String]): Invoker[R] = new StatementInvoker[R] {
        val getStatement                                    = statements.head
        protected def setParam(st: PreparedStatement)       = setParameter((), new PositionedParameters(st))
        protected def extractValue(rs: PositionedResult): R = getResult(rs)
      }
      protected[this] def createBuilder: collection.mutable.Builder[R, Vector[R]] = Vector.newBuilder[R]
    }
  }
  def asUpdate = as[Int](GetResult.GetUpdateValue).head

  /** Runs the query and reads each row as a `Seq` of all its column values, in column order. Use it
    * for queries whose columns are not known statically.
    *
    * The values are read with `ResultSet.getObject`, so their types are whatever the JDBC driver
    * returns for the column type (e.g. a numeric column may be read as `java.lang.Integer` or
    * `java.math.BigDecimal`). SQL NULL is returned as `null`. */
  def asSeqOfAny: SqlStreamingAction[Vector[Seq[Any]], Seq[Any], Effect] =
    as(SQLActionBuilder.seqOfAny)

  /** Runs the query and reads each row as a `Map` from column label to value. Use it for queries
    * whose columns are not known statically.
    *
    * The keys are the labels reported by `ResultSetMetaData.getColumnLabel`, i.e. the `AS` alias if
    * there is one. Their case depends on the database (unquoted identifiers may come back upper-case
    * or lower-case). The map iterates in column order. If several columns have the same label, only
    * the first of them is included, like `ResultSet.findColumn` does. The values are read as in
    * [[asSeqOfAny]]. */
  def asMapOfAny: SqlStreamingAction[Vector[Map[String, Any]], Map[String, Any], Effect] =
    as(SQLActionBuilder.mapOfAny)

  def concat(b: SQLActionBuilder): SQLActionBuilder = {
    SQLActionBuilder(sql + b.sql, (p, pp) => {
      setParameter(p, pp)
      b.setParameter(p, pp)
    })
  }
  def stripMargin(marginChar: Char): SQLActionBuilder = copy(sql.stripMargin(marginChar))
  def stripMargin: SQLActionBuilder = copy(sql.stripMargin)
}
