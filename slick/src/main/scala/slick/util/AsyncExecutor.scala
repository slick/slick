package slick.util

import java.io.Closeable
import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent._
import scala.concurrent.{Promise, Future, ExecutionContext}
import scala.util.control.NonFatal

/** A connection pool for asynchronous execution of blocking I/O actions.
  * This is used for the asynchronous query execution API on top of blocking back-ends like JDBC. */
trait AsyncExecutor extends Closeable {
  /** An ExecutionContext for running Futures. */
  def executionContext: ExecutionContext
  /** Shut the thread pool down and try to stop running computations. The thread pool is
    * transitioned into a state where it will not accept any new jobs. */
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
    new AsyncExecutor {
      // Before init: 0, during init: 1, after init: 2, during/after shutdown: 3
      private[this] val state = new AtomicInteger(0)

      @volatile private[this] var executor: ThreadPoolExecutor = _

      lazy val executionContext = {
        if(!state.compareAndSet(0, 1))
          throw new IllegalStateException("Cannot initialize ExecutionContext; AsyncExecutor already shut down")
        val queue = queueSize match {
          case 0 => new SynchronousQueue[Runnable]
          case -1 => new LinkedBlockingQueue[Runnable]
          case n =>
            new ManagedArrayBlockingQueue[Runnable](n * 2) {
              def accept(r: Runnable, size: Int) = r match {
                case pr: PrioritizedRunnable if pr.highPriority => true
                case _ => size < n
              }
            }
        }
        val tf = new DaemonThreadFactory(name + "-")
        executor = new ThreadPoolExecutor(numThreads, numThreads, 1, TimeUnit.MINUTES, queue, tf)
        if(!state.compareAndSet(1, 2)) {
          executor.shutdownNow()
          throw new IllegalStateException("Cannot initialize ExecutionContext; AsyncExecutor shut down during initialization")
        }
        ExecutionContext.fromExecutorService(executor, loggingReporter)
      }
      def close(): Unit = if(state.getAndSet(3) == 2) {
        executor.shutdownNow()
        if(!executor.awaitTermination(30, TimeUnit.SECONDS))
          logger.warn("Abandoning ThreadPoolExecutor (not yet destroyed after 30 seconds)")
      }
    }
  }

  def default(name: String = "AsyncExecutor.default"): AsyncExecutor =
    apply(name, 20, 1000)


  trait PrioritizedRunnable extends Runnable {
    def highPriority: Boolean
  }

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

  /** An Executor which spawns a new daemon thread for each command. It is useful for wrapping
    * synchronous `close` calls for asynchronous `shutdown` operations. */
  private[slick] val shutdownExecutor: Executor = new Executor {
    def execute(command: Runnable): Unit = {
      val t = new Thread(command)
      t.setName("shutdownExecutor")
      t.setDaemon(true)
      t.start()
    }
  }

  val loggingReporter: Throwable => Unit = (t: Throwable) => {
    logger.warn("Execution of asynchronous I/O action failed", t)
  }
}
