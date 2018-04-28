package slick.test.jdbc

import java.lang.management.ManagementFactory
import java.util.concurrent.TimeUnit
import javax.management.ObjectName

import org.junit.Assert._
import org.junit.Test
import slick.jdbc.H2Profile.api._

import scala.concurrent.{Future, Await}
import scala.concurrent.duration.Duration
import scala.concurrent.ExecutionContext.Implicits.global
import scala.collection.JavaConverters._

class MBeansTest {

  @Test
  def testMBeans: Unit = {
    val poolName = "myMbeansDatabase"
    val db = Database.forConfig("mbeans")
    try {
      db.ioExecutionContext // force initialization of AsyncExecutor

      val mbeanServer = ManagementFactory.getPlatformMBeanServer
      val beanNames =
        mbeanServer.queryNames(new ObjectName("slick:*"), null).asScala ++
          mbeanServer.queryNames(new ObjectName("com.zaxxer.hikari:*"), null).asScala
      println("Bean Names:")
      beanNames.foreach(n => println(s"  $n"))

      val aeBeanName = new ObjectName(s"slick:type=AsyncExecutor,name=$poolName")
      val poolBeanName = new ObjectName(s"com.zaxxer.hikari:type=Pool ($poolName)")
      val poolConfigBeanName = new ObjectName(s"com.zaxxer.hikari:type=PoolConfig ($poolName)")
      mbeanServer.getMBeanInfo(aeBeanName)
      mbeanServer.getMBeanInfo(poolBeanName)
      mbeanServer.getMBeanInfo(poolConfigBeanName)

      Await.result(db.run(sqlu"""create alias sleep for "java.lang.Thread.sleep""""), Duration(10, TimeUnit.SECONDS))

      assertEquals(1, mbeanServer.getAttribute(aeBeanName, "MaxThreads"))
      mbeanServer.getAttribute(aeBeanName, "ActiveThreads") // we expect 1, since minThreads == maxThreads
      assertEquals(1000, mbeanServer.getAttribute(aeBeanName, "MaxQueueSize"))
      assertEquals(0, mbeanServer.getAttribute(aeBeanName, "QueueSize"))

      val actions = Seq(
        sql"select 1, sleep(1000)".as[Int],
        sql"select 1".as[Int],
        sql"select 1".as[Int],
        sql"select 1".as[Int]
      )
      val fs = actions.map(db.run)
      val size = mbeanServer.getAttribute(aeBeanName, "QueueSize")
      // Usually we expect 3 but under high load it's possible that the first action is still in the queue
      assertTrue(s"size should be 3 or 4, is $size", size == 3 || size == 4)

      Await.result(Future.sequence(fs), Duration(10, TimeUnit.SECONDS))
    } finally db.close()
  }
}
