import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global

import slick.jdbc.{H2Profile, SQLiteProfile}
import slick.jdbc.JdbcBackend.Database

/** Run Slick code with multiple profiles using the Cake pattern. */
object MultiDBCakeExample extends App {

  def run(dal: DAL, db: Database): Future[Unit] = {
    import dal.profile.api._

    println("Running test against " + dal.profile)
    val dbio = for {
      _ <- dal.create

      // Creating our default picture
      defaultPic <- dal.insert(Picture("http://pics/default")): DBIOAction[Picture, NoStream, Effect.All]
      _ = println("- Inserted picture: " + defaultPic)

      // Inserting users
      u1 <- dal.insert(User("name1", defaultPic))
      _ = println("- Inserted user: " + u1)
      u2 <- dal.insert(User("name2", Picture("http://pics/2")))
      _ = println("- Inserted user: " + u2)
      u3 <- dal.insert(User("name3", defaultPic))
      _ = println("- Inserted user: " + u3)
      pictures <- dal.pictures.result
      _ = println("- All pictures: " + pictures)
      users <- dal.users.result
      _ = println("- All users: " + users)
    } yield ()
    db.run(dbio.withPinnedSession)
  }

  try {
    val f = {
      val h2db = Database.forConfig("h2")
      run(new DAL(H2Profile), h2db).andThen { case _ => h2db.close }
    }.flatMap { _ =>
      val sqlitedb = Database.forConfig("sqlite")
      run(new DAL(SQLiteProfile), sqlitedb).andThen { case _ => sqlitedb.close }
    }

    Await.result(f, Duration.Inf)
  } finally Util.unloadDrivers

}
