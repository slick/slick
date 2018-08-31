import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global

//#imports
import slick.dbio.DBIO
import slick.jdbc.JdbcBackend.Database
//#imports
import slick.jdbc.{H2Profile, SQLiteProfile}

/** Run SLICK code with multiple profiles. */
object MultiDBExample extends App {

  //#run
  def run(dao: DAO, db: Database): Future[Unit] = {
    val h = new DAOHelper(dao)
    println("Using profile " + dao.profile)
  //#run
    db.run(DBIO.seq(
      dao.create,
      dao.insert("foo", "bar"),
      dao.get("foo").map(r => println("- Value for key 'foo': " + r)),
      dao.get("baz").map(r => println("- Value for key 'baz': " + r)),
      h.dao.getFirst(h.restrictKey("foo", dao.props)).map(r => println("- Using the helper: " + r))
    ).withPinnedSession)
  }

  try {
    //#create
    val f = {
      val h2db = Database.forConfig("h2")
      run(new DAO(H2Profile), h2db).andThen { case _ => h2db.close }
    }.flatMap { _ =>
      val sqlitedb = Database.forConfig("sqlite")
      run(new DAO(SQLiteProfile), sqlitedb).andThen { case _ => sqlitedb.close }
    }
    //#create

    Await.result(f, Duration.Inf)
  } finally Util.unloadDrivers
}
