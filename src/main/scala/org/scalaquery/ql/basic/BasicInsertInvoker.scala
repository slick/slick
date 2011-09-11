package org.scalaquery.ql.basic

import annotation.implicitNotFound
import java.sql.Statement
import org.scalaquery.SQueryException
import org.scalaquery.ql.{ColumnBase, Query, Unpackable, Unpack}
import org.scalaquery.session.{Session, PositionedParameters}

class BasicInsertInvoker[T, U] (unpackable: Unpackable[T, U], profile: BasicProfile) {

  lazy val insertStatement = profile.buildInsertStatement(unpackable.value)
  def insertStatementFor[TT](query: Query[TT, U]): String = profile.buildInsertStatement(unpackable.value, query).sql
  def insertStatementFor[TT](c: TT)(implicit unpack: Unpack[TT, U]): String = insertStatementFor(Query(c))

  def useBatchUpdates(implicit session: Session) = session.capabilities.supportsBatchUpdates

  /**
   * Insert a single row.
   */
  def insert[V, TT](value: V)(implicit ev: PackedUnpackedUnion[TT, U, V], session: Session): Int =
    ev.fold(u => insertValue(u), (t, unpack) => insertExpr(t)(unpack, session))(value)

  def insertValue(value: U)(implicit session: Session): Int = session.withPreparedStatement(insertStatement) { st =>
    st.clearParameters()
    unpackable.linearizer.setParameter(profile, new PositionedParameters(st), Some(value))
    st.executeUpdate()
  }

  def insertExpr[TT](c: TT)(implicit unpack: Unpack[TT, U], session: Session): Int =
    insert(Query(c)(unpack))(session)

  /**
   * Insert multiple rows. Uses JDBC's batch update feature if supported by
   * the JDBC driver. Returns Some(rowsAffected), or None if the database
   * returned no row count for some part of the batch. If any part of the
   * batch fails, an exception thrown.
   */
  def insertAll(values: U*)(implicit session: Session): Option[Int] = {
    if(!useBatchUpdates || (values.isInstanceOf[IndexedSeq[_]] && values.length < 2))
      Some( (0 /: values) { _ + insertValue(_) } )
    else session.withTransaction {
      session.withPreparedStatement(insertStatement) { st =>
        st.clearParameters()
        for(value <- values) {
          unpackable.linearizer.setParameter(profile, new PositionedParameters(st), Some(value))
          st.addBatch()
        }
        var unknown = false
        var count = 0
        for((res, idx) <- st.executeBatch().zipWithIndex) res match {
          case Statement.SUCCESS_NO_INFO => unknown = true
          case Statement.EXECUTE_FAILED =>
            throw new SQueryException("Failed to insert row #" + (idx+1))
          case i => count += i
        }
        if(unknown) None else Some(count)
      }
    }
  }

  def insert[TT](query: Query[TT, U])(implicit session: Session): Int = {
    val sbr = profile.buildInsertStatement(unpackable.value, query)
    session.withPreparedStatement(insertStatementFor(query)) { st =>
      st.clearParameters()
      sbr.setter(new PositionedParameters(st), null)
      st.executeUpdate()
    }
  }

  def insertInvoker: this.type = this
}

@implicitNotFound(msg = "union type mismatch;\n found   : ${T}\n required: ${U}\n or      : ${P} with evidence Unpack[${P}, ${U}]")
trait PackedUnpackedUnion[P, U, T] {
  def fold[R](f: U => R, g: (P, Unpack[P, U]) => R)(v: T): R
}

object PackedUnpackedUnion extends PackedUnpackedUnionLowPriority {
  implicit def packedUnpackedUnionTypeU[P, U, T <: U] = new PackedUnpackedUnion[P, U, T] {
    def fold[R](f: U => R, g: (P, Unpack[P, U]) => R)(v: T): R = f(v)
  }
}

class PackedUnpackedUnionLowPriority {
  implicit def packedUnpackedUnionTypeP[P, U, T <: P](implicit ev: Unpack[P, U]) = new PackedUnpackedUnion[P, U, T] {
    def fold[R](f: U => R, g: (P, Unpack[P, U]) => R)(v: T): R = g(v, ev)
  }
}
