import scala.language.higherKinds

/** Common functionality that needs to work with types from the DAO
  * but in a DAO-independent way.
  */
//#daohelper
class DAOHelper(val dao: DAO) {
  import dao.profile.api._

  def restrictKey[C[_]](s: String, q: Query[DAO#Props, _, C]) =
    q.filter(_.key === s)
}
//#daohelper
