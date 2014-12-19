package scala.slick.jdbc

import scala.language.experimental.macros
import scala.language.implicitConversions
import scala.reflect.macros.Context
import java.sql.PreparedStatement

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

class SQLInterpolation(val s: StringContext) extends AnyVal {
  /** Build a SQLInterpolationResult via string interpolation */
  def sql(param: Any*) = macro SQLInterpolation.sqlImpl
  /** Build a StaticQuery for an UPDATE statement via string interpolation */
  def sqlu(param: Any*) = macro SQLInterpolation.sqluImpl
  /** Build an Invoker for a statement with computed types via string interpolation */
  def tsql(param: Any*) = macro SQLInterpolation.tsqlImpl
}

/**
 * Implementation of SQLInterpolation macros based on code in TypedStaticQuery object
 */
object SQLInterpolation {
  def sqlImpl(ctxt: Context)(param: ctxt.Expr[Any]*): ctxt.Expr[SQLInterpolationResult] = {
    import ctxt.universe._
    import TypedStaticQuery.{MacroConnectionHelper, MacroTreeBuilderHelper}

    val macroConnHelper = new MacroConnectionHelper(ctxt)

    val macroTreeBuilder = new {
      val c: ctxt.type = ctxt
      val resultTypes = Nil
      val paramsList = param.toList
      val queryParts = macroConnHelper.queryParts
    } with MacroTreeBuilderHelper

    reify {
      SQLInterpolationResult(
        ctxt.Expr[String]            (macroTreeBuilder.query    ).splice,
        ctxt.Expr[SetParameter[Unit]](macroTreeBuilder.pconvTree).splice
      )
    }
  }

  def sqluImpl(ctxt: Context)(param: ctxt.Expr[Any]*): ctxt.Expr[StaticQuery[Unit, Int]] = {
    import ctxt.universe._
    import TypedStaticQuery.{MacroConnectionHelper, MacroTreeBuilderHelper}

    val macroConnHelper = new MacroConnectionHelper(ctxt)

    val macroTreeBuilder = new {
      val c: ctxt.type = ctxt
      val resultTypes = Nil
      val paramsList = param.toList
      val queryParts = macroConnHelper.queryParts
    } with MacroTreeBuilderHelper

    reify {
      val res: SQLInterpolationResult = SQLInterpolationResult(
        ctxt.Expr[String]            (macroTreeBuilder.query    ).splice,
        ctxt.Expr[SetParameter[Unit]](macroTreeBuilder.pconvTree).splice
      )
      res.asUpdate
    }
  }

  def tsqlImpl(ctxt: Context)(param: ctxt.Expr[Any]*): ctxt.Expr[Invoker[Any]] = {
    import ctxt.universe._
    import TypedStaticQuery.{MacroConnectionHelper, MacroTreeBuilderHelper}

    val macroConnHelper = new MacroConnectionHelper(ctxt)

    val rTypes = macroConnHelper.configHandler.connection withSession {
      _.withPreparedStatement(macroConnHelper.rawQuery) {
        _.getMetaData match {
          case null => List()
          case resultMeta => List.tabulate(resultMeta.getColumnCount) { i =>
            meta.jdbcTypeToScala(resultMeta.getColumnType(i + 1))
          }
        }
      }
    }

    val macroTreeBuilder = new {
      val c: ctxt.type = ctxt
      val resultTypes = rTypes
      val paramsList = param.toList
      val queryParts = macroConnHelper.queryParts
    } with MacroTreeBuilderHelper

    reify {
      val rconv = ctxt.Expr[GetResult[Any]](macroTreeBuilder.rconvTree).splice
      val res: SQLInterpolationResult = SQLInterpolationResult(
        ctxt.Expr[String]            (macroTreeBuilder.query    ).splice,
        ctxt.Expr[SetParameter[Unit]](macroTreeBuilder.pconvTree).splice
      )
      res.as(rconv).apply(())
    }
  }
}

/**
 * Results of SQLInterpolation macros is SQLInterpolationResult objects
 */
case class SQLInterpolationResult(query: String, sp: SetParameter[Unit]) {
  def as[R](implicit rconv: GetResult[R]): StaticQuery[Unit, R] = new StaticQuery[Unit, R](query, sp, rconv)
  def asUpdate = as[Int](GetResult.GetUpdateValue)
}
