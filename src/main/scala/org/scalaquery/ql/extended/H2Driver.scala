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

  override def createQueryBuilder(query: Query[_, _], nc: NamingContext) = new H2QueryBuilder(query, nc, None, this)
}

object H2Driver extends H2Driver

class H2QueryBuilder(_query: Query[_, _], _nc: NamingContext, parent: Option[BasicQueryBuilder], profile: H2Driver)
extends BasicQueryBuilder(_query, _nc, parent, profile) {

  import ExtendedQueryOps._

  override type Self = H2QueryBuilder
  override protected val mayLimit0 = false
  override protected val concatOperator = Some("||")

  protected def createSubQueryBuilder(query: Query[_, _], nc: NamingContext) =
    new H2QueryBuilder(query, nc, Some(this), profile)

  override protected def innerExpr(c: Node, b: SQLBuilder): Unit = c match {
    case Sequence.Nextval(seq) => b += "nextval(schema(), '" += seq.name += "')"
    case Sequence.Currval(seq) => b += "currval(schema(), '" += seq.name += "')"
    case _ => super.innerExpr(c, b)
  }

  override protected def appendLimitClause(b: SQLBuilder) = query.typedModifiers[TakeDrop].lastOption.foreach {
    case TakeDrop(Some(0), _) => // handled in innerBuildSelectNoRewrite
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
