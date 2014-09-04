package scala.slick.util

import java.io.Closeable
import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.{ExecutorService, ArrayBlockingQueue, LinkedBlockingQueue, SynchronousQueue, ThreadFactory, TimeUnit, ThreadPoolExecutor, Executors}
import scala.concurrent.ExecutionContext
import scala.concurrent.duration.Duration
import com.typesafe.config.Config
import ConfigExtensionMethods._

/** A connection pool for asynchronous execution of blocking I/O actions.
  * This is used for the asynchronous query execution API on top of blocking back-ends like JDBC. */
trait AsyncExecutor extends Closeable {
  /** An [[scala.concurrent.ExecutionContext]] for running Futures. */
  def executionContext: ExecutionContext
  /** Shut the thread pool down and try to stop running computations. */
  def close(): Unit
}

object AsyncExecutor extends Logging {
  /** Create an [[AsyncExecutor]] with a thread pool suitable for blocking
    * I/O. New threads are created as daemon threads.
    *
    * @param name A prefix to use for the names of the created threads.
    * @param maxThreads The maximum number of threads in the pool.
    * @param coreThreads The desired number of threads beyond which queueing is prefered over
    *                    creating new threads.
    * @param keepAlive The duration after which excess threads (beyond `coreThreads`) are
    *                  shut down.
    * @param queueSize The size of the job queue, 0 for direct hand-off or -1 for unlimited size. */
  def apply(name: String, maxThreads: Int, coreThreads: Int, keepAlive: Duration, queueSize: Int): AsyncExecutor = {
    val queue = queueSize match {
      case 0 => new SynchronousQueue[Runnable]
      case -1 => new LinkedBlockingQueue[Runnable]
      case n => new ArrayBlockingQueue[Runnable](n)
    }
    val executor = new ThreadPoolExecutor(coreThreads, maxThreads,
      keepAlive.length, keepAlive.unit,
      queue,
      new DaemonThreadFactory(name + "-"))
    new AsyncExecutor {
      val executionContext = ExecutionContext.fromExecutorService(executor, loggingReporter)
      def close(): Unit = executor.shutdownNow()
    }
  }

  /** Create an [[AsyncExecutor]] from a [[Config]] object. See
    * [[scala.slick.jdbc.JdbcBackend.DatabaseFactoryDef.forConfig()]] for the supported
    * configuration keys. */
  def apply(name: String, config: Config, maxThreadsDefault: Int): AsyncExecutor = {
    val maxThreads = config.getIntOr("max", maxThreadsDefault)
    val coreThreads = config.getIntOr("core", maxThreads)
    val keepAlive = config.getDurationOr("keepAlive", Duration(1, TimeUnit.MINUTES))
    val queueSize = config.getIntOr("queueSize", 1000)
    apply(name, maxThreads, coreThreads, keepAlive, queueSize)
  }

  def default(name: String = "AsyncExecutor.default"): AsyncExecutor =
    apply(name, 30, 30, Duration(1, TimeUnit.MINUTES), 1000)

  private class DaemonThreadFactory(namePrefix: String) extends ThreadFactory {
    private[this] val group = Option(System.getSecurityManager).fold(Thread.currentThread.getThreadGroup)(_.getThreadGroup)
    private[this] val threadNumber = new AtomicInteger(1)

    def newThread(r: Runnable): Thread = {
      val t = new Thread(group, r, namePrefix + threadNumber.getAndIncrement, 0)
      if(!t.isDaemon) t.setDaemon(true)
      if(t.getPriority != Thread.NORM_PRIORITY) t.setPriority(Thread.NORM_PRIORITY)
      t
    }
  }

  val loggingReporter: Throwable => Unit = (t: Throwable) => {
    logger.warn("Execution of asynchronous I/O action failed", t)
  }
}
