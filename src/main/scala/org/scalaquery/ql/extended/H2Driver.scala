package org.scalaquery.ql.extended

import org.scalaquery.ql._
import org.scalaquery.ql.basic._
import org.scalaquery.ast._
import org.scalaquery.util._

class H2Driver extends ExtendedProfile { self =>

  type ImplicitT = ExtendedImplicitConversions[H2Driver]
  type TypeMapperDelegatesT = BasicTypeMapperDelegates

  val Implicit = new ExtendedImplicitConversions[H2Driver] {
    implicit val scalaQueryDriver = self
  }

  val typeMapperDelegates = new BasicTypeMapperDelegates {}
  override val sqlUtils = new H2SQLUtils

  override def createQueryBuilder(query: Query[_, _]) = new H2QueryBuilder(query, this)
}

object H2Driver extends H2Driver

class H2QueryBuilder(_query: Query[_, _], profile: H2Driver) extends BasicQueryBuilder(_query, profile) {

  override protected val mayLimit0 = false
  override protected val concatOperator = Some("||")

  override protected def expr(n: Node, b: SQLBuilder): Unit = n match {
    case Sequence.Nextval(seq) => b += "nextval(schema(), '" += seq.name += "')"
    case Sequence.Currval(seq) => b += "currval(schema(), '" += seq.name += "')"
    case _ => super.expr(n, b)
  }

  override protected def appendTakeDropClause(take: Option[Int], drop: Option[Int], b: SQLBuilder) = (take, drop) match {
    case (Some(t), Some(d)) => b += " LIMIT " += t += " OFFSET " += d
    case (Some(t), None) => b += " LIMIT " += t
    case (None, Some(d)) => b += " LIMIT 0 OFFSET " += d
    case _ =>
  }
}

class H2SQLUtils extends BasicSQLUtils {
  override def mapTypeName(tmd: TypeMapperDelegate[_]): String = tmd.sqlType match {
    case java.sql.Types.VARCHAR => "VARCHAR"
    case _ => super.mapTypeName(tmd)
  }
}
