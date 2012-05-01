package scala.slick.driver

import scala.slick.ql.Query
import scala.slick.session.{Session, PositionedParameters}
import scala.slick.util.RecordLinearizer

class BasicUpdateInvoker[T] (query: Query[_, T], profile: BasicProfile) {

  protected lazy val built = profile.buildUpdateStatement(query)

  def updateStatement = getStatement

  protected def getStatement = built.sql

  def update(value: T)(implicit session: Session): Int = session.withPreparedStatement(updateStatement) { st =>
    st.clearParameters
    val pp = new PositionedParameters(st)
    built.linearizer.narrowedLinearizer.asInstanceOf[RecordLinearizer[T]].setParameter(profile, pp, Some(value))
    built.setter(pp, null)
    st.executeUpdate
  }

  def updateInvoker: this.type = this
}
