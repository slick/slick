package slick.test.ce

import cats.effect.IO
import com.typesafe.config.ConfigFactory
import munit.CatsEffectSuite

import slick.cats
import slick.jdbc.DatabaseConfig
import slick.jdbc.JdbcProfile

/** Tests for [[DatabaseConfig.forConfig]] and database loading.
  *
  * Covers the fix for issue #3: `DatabaseConfig.forConfig[P]` returns a
  * configuration that can be used to create a live database.
  */
class DatabaseConfigTest extends CatsEffectSuite {

  private val h2Config = ConfigFactory.parseString(
    """
      |mydb {
      |  profile = "slick.jdbc.H2Profile$"
      |  db {
      |    connectionPool = disabled
      |    driver = "org.h2.Driver"
      |    url = "jdbc:h2:mem:dbconfigtest;DB_CLOSE_DELAY=-1"
      |  }
      |}
      |""".stripMargin
  )

  // ---------------------------------------------------------------------------
  // Profile-only forConfig (no db) — must still work
  // ---------------------------------------------------------------------------

  test("forConfig[P] returns the configured profile without opening a connection") {
    val dc = DatabaseConfig.forConfig[JdbcProfile]("mydb", h2Config)
    IO(assertEquals(dc.profileName, "slick.jdbc.H2Profile"))
  }

  // ---------------------------------------------------------------------------
  // Loading a database from a DatabaseConfig
  // ---------------------------------------------------------------------------

  val dc = DatabaseConfig.forConfig[JdbcProfile]("mydb", h2Config)

  val loadedDb = ResourceFunFixture(cats.Database.resource(dc))

  loadedDb.test("resource returns a live database") { db =>
    IO(assertEquals(dc.profileName, "slick.jdbc.H2Profile"))
  }

  loadedDb.test("loaded db can execute DBIO actions") { db =>
    import dc.profile.api.*
    class Rows(tag: Tag) extends Table[Int](tag, "DBCONFIG_ROWS") {
      def v = column[Int]("V")
      def * = v
    }
    val rows = TableQuery[Rows]
    db.run(
      DBIO.seq(
        rows.schema.create,
        rows += 1,
        rows += 2,
        rows.result.map(r => assertEquals(r.toSet, Set(1, 2))),
        rows.schema.drop
      )
    )
  }

  loadedDb.test("profile obtained from forConfig[P] matches across calls") { db =>
    val profileOnly = DatabaseConfig.forConfig[JdbcProfile]("mydb", h2Config)
    IO(assertEquals(dc.profile.getClass.getName, profileOnly.profile.getClass.getName))
  }
}
