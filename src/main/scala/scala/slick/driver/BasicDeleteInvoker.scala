package scala.slick.driver

import scala.slick.ql.Query
import scala.slick.session.{PositionedParameters, Session}

class BasicDeleteInvoker (query: Query[_, _], profile: BasicProfile) {

  protected lazy val built = profile.buildDeleteStatement(query)

  def deleteStatement = built.sql

  def delete(implicit session: Session): Int = session.withPreparedStatement(deleteStatement) { st =>
    built.setter(new PositionedParameters(st), null)
    st.executeUpdate
  }

  def deleteInvoker: this.type = this
}
