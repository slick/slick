package slick.jdbc

final class JdbcDatabaseConfig[P <: JdbcProfile](
  val profile: P,
  val source: JdbcDataSource,
  val maxConnections: Option[Int]
) {

  def profileName: String = profile.getClass.getName.stripSuffix("$")

  def profileIsObject: Boolean = profile.getClass.getName.endsWith("$")
}
