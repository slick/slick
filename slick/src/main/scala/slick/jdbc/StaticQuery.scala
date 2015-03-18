package slick.jdbc

import java.net.URI

import com.typesafe.config.ConfigException

import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.language.experimental.macros
import scala.language.implicitConversions
import scala.reflect.ClassTag
import scala.reflect.macros.Context
import scala.collection.mutable.ArrayBuffer

import java.sql.PreparedStatement

import slick.SlickException
import slick.backend.{DatabaseConfig, StaticDatabaseConfigMacros, StaticDatabaseConfig}
import slick.dbio.{NoStream, Effect}
import slick.driver.JdbcProfile
import slick.profile.{SqlAction, SqlStreamingAction}


///////////////////////////////////////////////////////////////////////////////// Invoker-based API

/** A builder for Plain SQL queries. */
class StaticQuery[-P,+R](query: String, pconv: SetParameter[P], rconv: GetResult[R]) extends (P => Invoker[R]) {
  /** Append a string to this query */
  def + (s: String) = new StaticQuery(query + s, pconv, rconv)

  /** Append a bind variable to this query */
  def +? [T](v: T)(implicit p: SetParameter[T]) = new StaticQuery(query + '?', new SetParameter[P] {
    def apply(param: P, pp: PositionedParameters) {
      pconv(param, pp)
      p(v, pp)
    }
  }, rconv)

  def apply(param: P): StaticQueryInvoker[P, R] = new StaticQueryInvoker[P, R](query, pconv, param, rconv)
}

object StaticQuery {
  def apply[R](implicit conv: GetResult[R]) = queryNA("")
  def apply[P, R](implicit pconv1: SetParameter[P],  rconv: GetResult[R]) = query[P,R]("")
  def u = updateNA("")
  def u1[P](implicit pconv1: SetParameter[P]) = update[P]("")

  def query[P,R](query: String)(implicit pconv: SetParameter[P], rconv: GetResult[R]) =
    new StaticQuery[P, R](query, pconv, rconv)

  def queryNA[R](query: String)(implicit rconv: GetResult[R]) =
    new StaticQuery[Unit, R](query, SetParameter.SetUnit, rconv)

  def update[P](query: String)(implicit pconv: SetParameter[P]) =
    new StaticQuery[P, Int](query, pconv, GetResult.GetUpdateValue)

  def updateNA(query: String) =
    new StaticQuery[Unit, Int](query, SetParameter.SetUnit, GetResult.GetUpdateValue)

  @inline implicit def interpolation(s: StringContext) = new SQLInterpolation(s)

  /** Automatically apply a parameterless query */
  @inline implicit def staticQueryToInvoker[R](s: StaticQuery[Unit, R]): StaticQueryInvoker[Unit, R] = s(())
}

/** Invoker for Plain SQL queries. */
class StaticQueryInvoker[-P, +R](val getStatement: String, pconv: SetParameter[P], param: P, rconv: GetResult[R]) extends StatementInvoker[R] {
  protected def setParam(st: PreparedStatement) = pconv(param, new PositionedParameters(st))
  protected def extractValue(rs: PositionedResult): R = rconv(rs)
}

@deprecated("Use the new Action-based Plain SQL API from driver.api instead", "3.0")
class SQLInterpolation(val s: StringContext) extends AnyVal {
  /** Build a SQLInterpolationResult via string interpolation */
  def sql(param: Any*) = macro SQLInterpolation.sqlImpl
  /** Build a StaticQuery for an UPDATE statement via string interpolation */
  def sqlu(param: Any*) = macro SQLInterpolation.sqluImpl
}

/**
 * Implementation of SQLInterpolation macros based on code in TypedStaticQuery object
 */
object SQLInterpolation {
  def sqlImpl(ctxt: Context)(param: ctxt.Expr[Any]*): ctxt.Expr[SQLInterpolationResult] = {
    import ctxt.universe._
    val macroTreeBuilder = new MacroTreeBuilder[ctxt.type](ctxt)(param.toList)
    reify {
      SQLInterpolationResult(
        ctxt.Expr[Seq[Any]]          (macroTreeBuilder.queryParts).splice,
        ctxt.Expr[SetParameter[Unit]](macroTreeBuilder.pconvTree).splice
      )
    }
  }

  def sqluImpl(ctxt: Context)(param: ctxt.Expr[Any]*): ctxt.Expr[StaticQuery[Unit, Int]] = {
    import ctxt.universe._
    val macroTreeBuilder = new MacroTreeBuilder[ctxt.type](ctxt)(param.toList)
    reify {
      val res: SQLInterpolationResult = SQLInterpolationResult(
        ctxt.Expr[Seq[Any]]          (macroTreeBuilder.queryParts).splice,
        ctxt.Expr[SetParameter[Unit]](macroTreeBuilder.pconvTree).splice
      )
      res.asUpdate
    }
  }
}

/**
 * Results of SQLInterpolation macros is SQLInterpolationResult objects
 */
@deprecated("Use the new Action-based Plain SQL API from driver.api instead", "3.0")
case class SQLInterpolationResult(queryParts: Seq[Any], sp: SetParameter[Unit]) {
  private[this] val query =
    if(queryParts.length == 1 && queryParts(0).isInstanceOf[String]) queryParts(0).asInstanceOf[String]
    else queryParts.iterator.map(String.valueOf).mkString
  def as[R](implicit rconv: GetResult[R]): StaticQuery[Unit, R] = new StaticQuery[Unit, R](query, sp, rconv)
  def asUpdate = as[Int](GetResult.GetUpdateValue)
}

////////////////////////////////////////////////////////////////////////////////// Action-based API

class ActionBasedSQLInterpolation(val s: StringContext) extends AnyVal {
  import ActionBasedSQLInterpolation._
  
  /** Build a SQLActionBuilder via string interpolation */
  def sql(param: Any*): SQLActionBuilder = macro sqlImpl
  /** Build an Action for an UPDATE statement via string interpolation */
  def sqlu(param: Any*): SqlAction[Int, NoStream, Effect] = macro sqluImpl
  /** Build an Invoker for a statement with computed types via string interpolation */
  def tsql(param: Any*): SqlStreamingAction[Vector[Any], Any, Effect] = macro tsqlImpl
}

object ActionBasedSQLInterpolation {
  def sqlImpl(ctxt: Context)(param: ctxt.Expr[Any]*): ctxt.Expr[SQLActionBuilder] = {
    import ctxt.universe._
    val macroTreeBuilder = new MacroTreeBuilder[ctxt.type](ctxt)(param.toList)
    reify {
      SQLActionBuilder(
        ctxt.Expr[Seq[Any]]          (macroTreeBuilder.queryParts).splice,
        ctxt.Expr[SetParameter[Unit]](macroTreeBuilder.pconvTree).splice
      )
    }
  }

  def sqluImpl(ctxt: Context)(param: ctxt.Expr[Any]*): ctxt.Expr[SqlAction[Int, NoStream, Effect]] = {
    import ctxt.universe._
    val macroTreeBuilder = new MacroTreeBuilder[ctxt.type](ctxt)(param.toList)
    reify {
      val res: SQLActionBuilder = SQLActionBuilder(
        ctxt.Expr[Seq[Any]]          (macroTreeBuilder.queryParts).splice,
        ctxt.Expr[SetParameter[Unit]](macroTreeBuilder.pconvTree).splice
      )
      res.asUpdate
    }
  }
  def tsqlImpl(ctxt: Context)(param: ctxt.Expr[Any]*): ctxt.Expr[SqlStreamingAction[Vector[Any], Any, Effect]] = {
    import ctxt.universe._
    val macroTreeBuilder = new MacroTreeBuilder[ctxt.type](ctxt)(param.toList)

    val uri = StaticDatabaseConfigMacros.getURI(ctxt)
    //TODO The database configuration and connection should be cached for subsequent macro invocations
    val dc =
      try DatabaseConfig.forURI[JdbcProfile](new URI((uri))) catch {
        case ex @ (_: ConfigException | _: SlickException) =>
          ctxt.abort(ctxt.enclosingPosition, s"""Cannot load @StaticDatabaseConfig("$uri"): ${ex.getMessage}""")
      }
    val rTypes = try {
      val a = SimpleJdbcAction { ctx =>
        ctx.session.withPreparedStatement(macroTreeBuilder.staticQueryString) {
          _.getMetaData match {
            case null => Vector()
            case resultMeta => Vector.tabulate(resultMeta.getColumnCount) { i =>
              val modelBuilder = dc.driver.createModelBuilder(Nil, true)(scala.concurrent.ExecutionContext.global)
              modelBuilder.jdbcTypeToScala(resultMeta.getColumnType(i + 1))
            }
          }
        }
      }
      Await.result(dc.db.run(a), Duration.Inf)
    } finally Await.ready(dc.db.shutdown, Duration.Inf)

    reify {
      val rconv = ctxt.Expr[GetResult[Any]](macroTreeBuilder.rconvTree(rTypes)).splice
      val res: SQLActionBuilder = SQLActionBuilder(
        ctxt.Expr[Seq[Any]]          (macroTreeBuilder.queryParts).splice,
        ctxt.Expr[SetParameter[Unit]](macroTreeBuilder.pconvTree).splice
      )
      res.as(rconv)
    }
  }
}

case class SQLActionBuilder(queryParts: Seq[Any], unitPConv: SetParameter[Unit]) {
  def as[R](implicit rconv: GetResult[R]): SqlStreamingAction[Vector[R], R, Effect] = {
    val query =
      if(queryParts.length == 1 && queryParts(0).isInstanceOf[String]) queryParts(0).asInstanceOf[String]
      else queryParts.iterator.map(String.valueOf).mkString
    new StreamingInvokerAction[Vector[R], R, Effect] {
      def statements = List(query)
      protected[this] def createInvoker(statements: Iterable[String]) = new StaticQueryInvoker[Unit, R](statements.head, unitPConv, (), rconv)
      protected[this] def createBuilder = Vector.newBuilder[R]
    }
  }
  def asUpdate = as[Int](GetResult.GetUpdateValue).head
}
