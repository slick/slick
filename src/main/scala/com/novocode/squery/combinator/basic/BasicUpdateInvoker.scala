package com.novocode.squery.combinator.basic

import com.novocode.squery.combinator.{Query, Projection, NamingContext}
import com.novocode.squery.session.{Session, CloseableIterator, ReadAheadIterator, PositionedParameters}

class BasicUpdateInvoker[T <: Product] (query: Query[Projection[T]], profile: BasicProfile) {

  lazy val updateStatement = profile.buildUpdateStatement(query, NamingContext())

  def update(value: T)(implicit session: Session): Int = session.withPS(updateStatement) { st =>
    st.clearParameters
    query.value.setParameter(profile, new PositionedParameters(st), Some(value))
    st.executeUpdate
  }
}
