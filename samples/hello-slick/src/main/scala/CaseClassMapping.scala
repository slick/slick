import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import slick.jdbc.H2Profile.api._

object CaseClassMapping extends App {

  // the base query for the Users table
  val users = TableQuery[Users]

  val db = Database.forConfig("h2mem1")
  try {
    Await.result(db.run(DBIO.seq(
      // create the schema
      users.schema.create,

      // insert two User instances
      users += User("John Doe"),
      users += User("Fred Smith"),

      // print the users (select * from USERS)
      users.result.map(println)
    )), Duration.Inf)
  } finally db.close
}

case class User(name: String, id: Option[Int] = None)

class Users(tag: Tag) extends Table[User](tag, "USERS") {
  // Auto Increment the id primary key column
  //#autoInc
  def id = column[Int]("ID", O.PrimaryKey, O.AutoInc)
  //#autoInc
  // The name can't be null
  def name = column[String]("NAME")
  // the * projection (e.g. select * ...) auto-transforms the tupled
  // column values to / from a User
  //#mapTo
  def * = (name, id.?).mapTo[User]
  //#mapTo
}
