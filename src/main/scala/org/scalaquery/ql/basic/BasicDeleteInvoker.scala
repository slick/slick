package org.scalaquery.ql.basic

import org.scalaquery.ql.Query
import org.scalaquery.session.{PositionedParameters, Session}

class BasicDeleteInvoker (query: Query[_, _], profile: BasicProfile) {

  protected lazy val built = profile.buildDeleteStatement(query)

  def deleteStatement = built.sql

  def delete(implicit session: Session): Int = session.withPreparedStatement(deleteStatement) { st =>
    built.setter(new PositionedParameters(st), null)
    st.executeUpdate
  }

  def deleteInvoker: this.type = this
}
