package slick.test.jdbc.hikaricp

import java.sql.Blob
import javax.sql.rowset.serial.SerialBlob

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.syntax.parallel.*

import com.typesafe.slick.testkit.util.{AsyncTest, JdbcTestDB}
import org.junit.{After, Before, Test}
import slick.cats.Database
import slick.jdbc.DatabaseConfig
import slick.jdbc.H2Profile
import slick.jdbc.H2Profile.api._
import slick.lifted.{ProvenShape, TableQuery}

class SlickDeadlockTest extends AsyncTest[JdbcTestDB] {

  class TestTable(tag: Tag) extends Table[(Int)](tag, "SDL") {

    def id: Rep[Int] = column[Int]("ID")
    def * : ProvenShape[(Int)] = id

  }

  class BlobTable(tag: Tag) extends Table[(Int, Blob)](tag, "SDLIO") {
    def id = column[Int]("id")
    def data = column[Blob]("data")
    def * = (id, data)
  }


  var database: Database = _
  var dbClose: IO[Unit] = _
  val testTable: TableQuery[TestTable] = TableQuery[TestTable]
  val blobTable: TableQuery[BlobTable] = TableQuery[BlobTable]

  @Before
  def openDatabase() = {
    val dc = DatabaseConfig.forProfileConfig(H2Profile, "h2mem1")
    val (db, close) = Database.resource(dc).allocated.unsafeRunSync()
    database = db
    dbClose = close
    database.run((testTable.schema ++ blobTable.schema).create).unsafeRunSync()
  }

  @After
  def closeDatabase() = {
    database.run((testTable.schema ++ blobTable.schema).drop).unsafeRunSync()
    dbClose.unsafeRunSync()
  }


  @Test def slickDoesNotDeadlock(): Unit = {

    val tasks: Seq[IO[String]] = 1 to 51 map { i =>
      val action = { testTable += i }
        .flatMap { _ => testTable.length.result }
        .flatMap { _ => DBIO.successful(s"inserted value $i") }

      database.run(action.transactionally)
    }
    tasks.toList.parSequence.unsafeRunSync()
  }

  @Test def slickDoesNotDeadlockWithSleeps(): Unit = {
    val tasks: Seq[IO[Int]] = 1 to 21 map { c =>
      val action = sql"select $c".as[Int].head.map { i => Thread.sleep(if(c == 1) 100 else 200); i }
      database.run(action.transactionally)
    }
    tasks.toList.parSequence.unsafeRunSync()
  }

  @Test def slickDoesNotDeadlockWithIo(): Unit = {

    database.run((
        (blobTable += (1, new SerialBlob(Array[Byte](1,2,3)))) >>
        (blobTable += (2, new SerialBlob(Array[Byte](4,5)))) >>
        blobTable.result
    ).transactionally).unsafeRunSync()

    val tasks: Seq[IO[Vector[(Int, String)]]] = 1 to 51 map { _ =>
      materializeAsync[(Int, Blob), (Int, String)](
        database.stream(blobTable.result.transactionally),
        { case (id, data) => IO.pure((id, data.getBytes(1, data.length.toInt).mkString)) }
      )
    }

    tasks.toList.parSequence.unsafeRunSync()
  }

}
