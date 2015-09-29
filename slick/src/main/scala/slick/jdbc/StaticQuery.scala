package slick.jdbc

import java.net.URI

import com.typesafe.config.ConfigException
import slick.util.ClassLoaderUtil

import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.language.experimental.macros
import scala.language.implicitConversions
import scala.reflect.ClassTag
import scala.reflect.macros.{blackbox, whitebox}
import scala.collection.mutable.ArrayBuffer

import java.sql.PreparedStatement

import slick.SlickException
import slick.backend.{DatabaseConfig, StaticDatabaseConfigMacros, StaticDatabaseConfig}
import slick.dbio.{NoStream, Effect}
import slick.driver.JdbcProfile
import slick.profile.{SqlAction, SqlStreamingAction}

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
  def sqlImpl(ctxt: blackbox.Context)(param: ctxt.Expr[Any]*): ctxt.Expr[SQLActionBuilder] = {
    import ctxt.universe._
    val macroTreeBuilder = new MacroTreeBuilder[ctxt.type](ctxt)(param.toList)
    reify {
      SQLActionBuilder(
        ctxt.Expr[Seq[Any]]          (macroTreeBuilder.queryParts).splice,
        ctxt.Expr[SetParameter[Unit]](macroTreeBuilder.pconvTree).splice
      )
    }
  }

  def sqluImpl(ctxt: blackbox.Context)(param: ctxt.Expr[Any]*): ctxt.Expr[SqlAction[Int, NoStream, Effect]] = {
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
  def tsqlImpl(ctxt: whitebox.Context)(param: ctxt.Expr[Any]*): ctxt.Expr[SqlStreamingAction[Vector[Any], Any, Effect]] = {
    import ctxt.universe._
    val macroTreeBuilder = new MacroTreeBuilder[ctxt.type](ctxt)(param.toList)

    val uri = StaticDatabaseConfigMacros.getURI(ctxt)
    //TODO The database configuration and connection should be cached for subsequent macro invocations
    val dc =
      try DatabaseConfig.forURI[JdbcProfile](new URI(uri), ClassLoaderUtil.defaultClassLoader) catch {
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
              modelBuilder.jdbcTypeToScala(resultMeta.getColumnType(i + 1), resultMeta.getColumnTypeName(i + 1))
            }
          }
        }
      }
      Await.result(dc.db.run(a), Duration.Inf)
    } finally dc.db.close()

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
