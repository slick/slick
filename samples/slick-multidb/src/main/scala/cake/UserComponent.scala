import scala.concurrent.ExecutionContext.Implicits.global

/** A User contains a name, picture and ID */
case class User(name: String, picture: Picture, id: Option[Int] = None)

/** UserComponent provides database definitions for User objects */
//#outline
trait UserComponent { this: ProfileComponent with PictureComponent =>
  import profile.api._
//#outline

  class Users(tag: Tag) extends Table[(String, Int, Option[Int])](tag, "USERS") {
    def id = column[Option[Int]]("USER_ID", O.PrimaryKey, O.AutoInc)
    def name = column[String]("USER_NAME")
    def pictureId = column[Int]("PIC_ID")
    def * = (name, pictureId, id)
  }
  val users = TableQuery[Users]

  private val usersAutoInc =
    users.map(u => (u.name, u.pictureId)) returning users.map(_.id)

  def insert(user: User): DBIO[User] = for {
    //#insert
    pic <-
      if(user.picture.id.isEmpty) insert(user.picture)
      else DBIO.successful(user.picture)
    //#insert
    id <- usersAutoInc += (user.name, pic.id.get)
  } yield user.copy(picture = pic, id = id)
}
