import slick.driver.JdbcProfile

import scala.concurrent.{Future, Await}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import slick.backend.{DatabaseConfig, StaticDatabaseConfig}

@StaticDatabaseConfig("#tsql")
object TypedSQL extends App {
  val dc = DatabaseConfig.forAnnotation[JdbcProfile]
  import dc.driver.api._

  def getSuppliers(id: Int): DBIO[Seq[(Int, String, String, String, String, String)]] =
    tsql"select * from suppliers where id > $id"

  val db = dc.db
  try {

    val a: DBIO[Unit] =
      getSuppliers(50).map { s =>
        println("All suppliers > 50:")
        s.foreach(println)
      }

    val f: Future[Unit] = db.run(a)
    Await.result(f, Duration.Inf)
  } finally db.close
}
