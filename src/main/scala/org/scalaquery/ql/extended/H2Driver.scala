package org.scalaquery.ql.extended

import org.scalaquery.ql._
import org.scalaquery.ql.basic._
import org.scalaquery.util._

class H2Driver extends ExtendedProfile { self =>

  type ImplicitT = ExtendedImplicitConversions[H2Driver]
  type TypeMapperDelegatesT = BasicTypeMapperDelegates

  val Implicit = new ExtendedImplicitConversions[H2Driver] {
    implicit val scalaQueryDriver = self
  }

  val typeMapperDelegates = new BasicTypeMapperDelegates {}
  override val sqlUtils = new H2SQLUtils

  override def createQueryBuilder(query: Query[_], nc: NamingContext) = new H2QueryBuilder(query, nc, None, this)
}

object H2Driver extends H2Driver

class H2QueryBuilder(_query: Query[_], _nc: NamingContext, parent: Option[BasicQueryBuilder], profile: H2Driver)
extends BasicQueryBuilder(_query, _nc, parent, profile) {

  import ExtendedQueryOps._

  override type Self = H2QueryBuilder

  protected def createSubQueryBuilder(query: Query[_], nc: NamingContext) =
    new H2QueryBuilder(query, nc, Some(this), profile)

  override protected def innerBuildSelectNoRewrite(b: SQLBuilder, rename: Boolean) {
    query.typedModifiers[TakeDrop] match {
      case TakeDrop(Some(0), _) :: _ =>
        /* H2 ignores LIMIT 0 and treats negative limits as positive, so
         * we use this workaround to force the query to return no results */
        b += "SELECT * FROM ("
        super.innerBuildSelectNoRewrite(b, rename)
        b += ") WHERE FALSE"
      case _ =>
        super.innerBuildSelectNoRewrite(b, rename)
    }
  }

  override protected def innerExpr(c: Node, b: SQLBuilder): Unit = c match {
    case ColumnOps.Concat(l, r) => b += '('; expr(l, b); b += "||"; expr(r, b); b += ')'
    case Sequence.Nextval(seq) => b += "nextval(schema(), '" += seq.name += "')"
    case Sequence.Currval(seq) => b += "currval(schema(), '" += seq.name += "')"
    case _ => super.innerExpr(c, b)
  }

  override protected def appendClauses(b: SQLBuilder): Unit = {
    super.appendClauses(b)
    appendLimitClause(b)
  }

  protected def appendLimitClause(b: SQLBuilder): Unit = query.typedModifiers[TakeDrop].lastOption.foreach {
    case TakeDrop(Some(0), _) => // handled above in innerBuildSelectNoRewrite
    case TakeDrop(Some(t), Some(d)) => b += " LIMIT " += t += " OFFSET " += d
    case TakeDrop(Some(t), None) => b += " LIMIT " += t
    case TakeDrop(None, Some(d)) => b += " LIMIT 0 OFFSET " += d
    case _ =>
  }
}

class H2SQLUtils extends BasicSQLUtils {
  override def mapTypeName(tmd: TypeMapperDelegate[_]): String = tmd.sqlType match {
    case java.sql.Types.VARCHAR => "VARCHAR"
    case _ => super.mapTypeName(tmd)
  }
}
