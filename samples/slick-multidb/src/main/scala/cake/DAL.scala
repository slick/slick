import slick.jdbc.JdbcProfile

/** The Data Access Layer contains all components and a profile */
class DAL(val profile: JdbcProfile)
      extends UserComponent with PictureComponent with ProfileComponent {
  import profile.api._

  def create =
    (users.schema ++ pictures.schema).create
}
