package slick.test.jdbc.hikaricp

import java.sql.Blob
import java.util.concurrent.TimeUnit
import javax.sql.rowset.serial.SerialBlob

import com.typesafe.slick.testkit.util.{AsyncTest, JdbcTestDB}
import org.junit.{After, Before, Test}
import slick.jdbc.H2Profile.api._
import slick.lifted.{ProvenShape, TableQuery}

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

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
  val testTable: TableQuery[TestTable] = TableQuery[TestTable]
  val blobTable: TableQuery[BlobTable] = TableQuery[BlobTable]

  @Before
  def openDatabase() = {
    database = Database.forConfig("h2mem1")
    Await.result(database.run((testTable.schema ++ blobTable.schema).create), 2.seconds)
  }

  @After
  def closeDatabase() = {
    Await.result(database.run((testTable.schema ++ blobTable.schema).drop), 2.seconds)
    database.close()
  }


  @Test def slickDoesNotDeadlock(): Unit = {

    val tasks = 1 to 51 map { i =>
      val action = { testTable += i }
        .flatMap { _ => testTable.length.result }
        .flatMap { _ => DBIO.successful(s"inserted value $i") }

      database.run(action.transactionally)
    }
    Await.result(Future.sequence(tasks), Duration(100, TimeUnit.SECONDS))
  }

  @Test def slickDoesNotDeadlockWithSleeps(): Unit = {
    val tasks = 1 to 21 map { c =>
      val action = sql"select $c".as[Int].head.map { i => Thread.sleep(if(c == 1) 100 else 200); i }

      database.run(action.transactionally)
    }
    Await.result(Future.sequence(tasks), Duration(100, TimeUnit.SECONDS))

  }

  @Test def slickDoesNotDeadlockWithIo(): Unit = {

    Await.result(database.run((
        (blobTable += (1, new SerialBlob(Array[Byte](1,2,3)))) >>
        (blobTable += (2, new SerialBlob(Array[Byte](4,5)))) >>
        blobTable.result
    ).transactionally), Duration(20, TimeUnit.SECONDS))

    val tasks = 1 to 51 map { i =>
      materializeAsync[(Int, Blob), (Int, String)](database.stream(blobTable.result.transactionally, bufferNext = false),
        { case (id, data) => database.io((id, data.getBytes(1, data.length.toInt).mkString)) })
    }

    Await.result(Future.sequence(tasks), Duration(100, TimeUnit.SECONDS))
  }

}
