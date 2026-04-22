package slick.test.compile

import cats.effect.IO

import slick.cats.Database
import slick.ControlsConfig
import slick.jdbc.DatabaseConfig
import slick.jdbc.H2Profile

/** Compile-time test: verifies that the DatabaseConfig factory API produces a
  * db value whose type is anchored to the concrete profile, so that
  * profile.api._ imports and db.run(...) type-check without casts.
  *
  * This must compile on Scala 2.12, 2.13 and 3.x.
  *
  * Note: on Scala 2.12, P.backend.makeDatabase[IO](dc) returns F[dc.profile.backend.Database[F]]
  * (anchored to the dc instance's profile), not F[P.backend.Database[F]].
  * Users should use Database.resource(dc).use(...) or P.backend.makeDatabase[IO](config)
  * for profile-precise types on 2.12. The Database.resource pattern works uniformly.
  */
object DatabaseConfigApiTest {

  val P = H2Profile
  import P.api._

  // forConfig — Database.resource.use pattern works on all Scala versions
  locally {
    val dc = DatabaseConfig.forConfig[H2Profile]("mydb")
    val _result: IO[Int] = Database.resource(dc).use(db => db.run(DBIO.successful(1)))
  }

  // forProfileConfig with a stable profile val — Database.resource.use pattern
  locally {
    val dc = DatabaseConfig.forProfileConfig(P, "mydb")
    val _result: IO[Int] = Database.resource(dc).use(db => db.run(DBIO.successful(1)))
  }

  // forURL with a stable profile val — Database.resource.use pattern
  locally {
    val dc = DatabaseConfig.forURL(P, "jdbc:h2:mem:test")
    val _result: IO[Int] = Database.resource(dc).use(db => db.run(DBIO.successful(1)))
  }

  // forDataSource — Database.resource.use pattern
  locally {
    val ds = new org.h2.jdbcx.JdbcDataSource()
    val dc = DatabaseConfig.forDataSource(P, ds).withControls(ControlsConfig(maxConnections = 4))
    val _result: IO[Int] = Database.resource(dc).use(db => db.run(DBIO.successful(1)))
  }

  // forDriver — Database.resource.use pattern
  locally {
    val dc = DatabaseConfig.forDriver(P, new org.h2.Driver(), "jdbc:h2:mem:test")
    val _result: IO[Int] = Database.resource(dc).use(db => db.run(DBIO.successful(1)))
  }
}
