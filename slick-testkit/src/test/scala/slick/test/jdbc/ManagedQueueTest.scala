package slick.test.jdbc

import java.sql.Connection
import java.util.UUID

import com.typesafe.config.ConfigFactory
import org.junit.Test
import org.junit.Assert._
import slick.jdbc.H2Profile.api._
import slick.jdbc.{JdbcBackend, JdbcDataSource}
import slick.util.ClassLoaderUtil

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.util.Failure

class ManagedQueueTest {


  @Test
  def testBrokenConnectionWithStreamingAction() = {

    val config =
      ConfigFactory.parseString(
        """
          |dataSource {
          |  profile = "slick.jdbc.H2Profile$"
          |  db {
          |    connectionPool = disabled
          |    dataSourceClass = "slick.jdbc.DriverDataSource"
          |    properties = {
          |      driver = "org.h2.Driver"
          |      url = "jdbc:h2:mem:test;DB_CLOSE_DELAY=-1"
          |    }
          |  }
          |}
          |""".stripMargin)

    val dataSource = new JdbcDataSourceWrap(
      JdbcDataSource.forConfig(
        config.getConfig("dataSource.db"),
        driver = null,
        "test",
        ClassLoaderUtil.defaultClassLoader)
    )

    // only one thread and one connection available
    val asyncExecutor = AsyncExecutor("test", 1, 1, 1, 1)

    class T(tag: Tag) extends Table[Int](tag, "TableA") {
      def a = column[Int]("a")
      def * = a
    }
    val ts = TableQuery[T]

    val db = JdbcBackend.Database.forSource(dataSource, asyncExecutor)
    try {
      val values = Seq(2, 3, 1, 5, 4)
      // init schema and insert some data
      val initAction =
        for {
          _ <- ts.schema.create
          _ <- ts ++= values
        } yield ()
      Await.ready(db.run(initAction.transactionally), 5.seconds)

      // run stream in fail mode
      // before the fix for (https://github.com/slick/slick/issues/1875)
      // this would consume the single thread we have in AsyncExecutor
      dataSource.failMode()
      val streamResult = db.stream(ts.result).foreach(println)
      Await
        .ready(streamResult, 3.seconds)
        .onComplete {
          case Failure(ex) =>
            assertEquals("DB is not available!", ex.getMessage)
          case _ => fail("This was expect to fail")
        }

      // before the fix for (https://github.com/slick/slick/issues/1875)
      // this would have hung forever
      // the fix brings the managed queue back to not-paused state
      // which allows this next call to succeed
      dataSource.successMode()
      val seq = Await.result(db.run(ts.result), 3.seconds)
      assertEquals(values, seq)

    } finally {
      db.close
    }
  }

  /* JdbcDataSource wrap that will help us to simulate connection failures */
  class JdbcDataSourceWrap(underlying: JdbcDataSource) extends JdbcDataSource {
    private var _failMode = false

    def failMode() = _failMode = true
    def successMode() = _failMode = false

    override def createConnection(): Connection = {
      if (_failMode) throw new RuntimeException("DB is not available!")
      else underlying.createConnection()
    }

    override def close(): Unit = underlying.close()
    override val maxConnections: Option[Int] = underlying.maxConnections
  }


}
