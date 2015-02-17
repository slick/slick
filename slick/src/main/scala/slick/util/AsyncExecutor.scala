package slick.util

import java.io.Closeable
import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.{ExecutorService, ArrayBlockingQueue, LinkedBlockingQueue, SynchronousQueue, ThreadFactory, TimeUnit, ThreadPoolExecutor, Executors}
import scala.concurrent.ExecutionContext

/** A connection pool for asynchronous execution of blocking I/O actions.
  * This is used for the asynchronous query execution API on top of blocking back-ends like JDBC. */
trait AsyncExecutor extends Closeable {
  /** An ExecutionContext for running Futures. */
  def executionContext: ExecutionContext
  /** Shut the thread pool down and try to stop running computations. */
  def close(): Unit
}

object AsyncExecutor extends Logging {
  /** Create an [[AsyncExecutor]] with a thread pool suitable for blocking
    * I/O. New threads are created as daemon threads.
    *
    * @param name A prefix to use for the names of the created threads.
    * @param numThreads The number of threads in the pool.
    * @param queueSize The size of the job queue, 0 for direct hand-off or -1 for unlimited size. */
  def apply(name: String, numThreads: Int, queueSize: Int): AsyncExecutor = {
    val queue = queueSize match {
      case 0 => new SynchronousQueue[Runnable]
      case -1 => new LinkedBlockingQueue[Runnable]
      case n => new ArrayBlockingQueue[Runnable](n)
    }
    val tf = new DaemonThreadFactory(name + "-")
    val executor = new ThreadPoolExecutor(numThreads, numThreads, 1, TimeUnit.MINUTES, queue, tf)
    new AsyncExecutor {
      val executionContext = ExecutionContext.fromExecutorService(executor, loggingReporter)
      def close(): Unit = executor.shutdownNow()
    }
  }

  def default(name: String = "AsyncExecutor.default"): AsyncExecutor =
    apply(name, 20, 1000)

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
