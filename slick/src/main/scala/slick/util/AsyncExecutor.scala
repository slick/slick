package slick.util

import java.io.Closeable
import java.lang.management.ManagementFactory
import java.util.concurrent._
import java.util.concurrent.atomic.AtomicInteger

import scala.concurrent._
import scala.concurrent.duration._
import scala.util.control.NonFatal

import slick.util.AsyncExecutor.Priority

import javax.management.{InstanceNotFoundException, ObjectName}

/**
 * A connection pool for asynchronous execution of blocking I/O actions. This is used for the asynchronous query
 * execution API on top of blocking back-ends like JDBC.
 */
trait AsyncExecutor extends Closeable {

  /** An ExecutionContext for running Futures. */
  def executionContext: ExecutionContext

  /**
   * Shut the thread pool down and try to stop running computations. The thread pool is transitioned into a state where
   * it will not accept any new jobs.
   */
  def close(): Unit

  def prioritizedRunnable(
      priority: => Priority,
      run: AsyncExecutor.PrioritizedRunnable.SetConnectionReleased => Unit
  ): AsyncExecutor.PrioritizedRunnable =
    AsyncExecutor.PrioritizedRunnable(priority, run)
}

object AsyncExecutor extends Logging {

  /**
   * Create an [[AsyncExecutor]] with a thread pool suitable for blocking I/O. New threads are created as daemon
   * threads.
   *
   * @param name
   *   A prefix to use for the names of the created threads.
   * @param numThreads
   *   The number of threads in the pool.
   * @param queueSize
   *   The size of the job queue, 0 for direct hand-off or -1 for unlimited size.
   */
  def apply(name: String, numThreads: Int, queueSize: Int): AsyncExecutor =
    apply(
      name = name,
      minThreads = numThreads,
      maxThreads = numThreads,
      queueSize = queueSize,
      maxConnections = if (queueSize == -1) Int.MaxValue else numThreads,
      keepAliveTime = 1.minute,
      registerMbeans = false
    )

  /**
   * Create an [[AsyncExecutor]] with a thread pool suitable for blocking I/O. New threads are created as daemon
   * threads.
   *
   * @param name
   *   A prefix to use for the names of the created threads.
   * @param minThreads
   *   The number of core threads in the pool.
   * @param maxThreads
   *   The maximum number of threads in the pool.
   * @param queueSize
   *   The size of the job queue, 0 for direct hand-off or -1 for unlimited size.
   * @param maxConnections
   *   The maximum number of configured connections for the connection pool. The underlying ThreadPoolExecutor will not
   *   pick up any more work when all connections are in use. It will resume as soon as a connection is released again
   *   to the pool
   */
  def apply(name: String, minThreads: Int, maxThreads: Int, queueSize: Int, maxConnections: Int): AsyncExecutor =
    apply(
      name,
      minThreads = minThreads,
      maxThreads = maxThreads,
      queueSize = queueSize,
      maxConnections = maxConnections,
      keepAliveTime = 1.minute,
      registerMbeans = false
    )

  /**
   * Create an [[AsyncExecutor]] with a thread pool suitable for blocking I/O. New threads are created as daemon
   * threads.
   *
   * @param name
   *   A prefix to use for the names of the created threads.
   * @param minThreads
   *   The number of core threads in the pool.
   * @param maxThreads
   *   The maximum number of threads in the pool.
   * @param queueSize
   *   The size of the job queue, 0 for direct hand-off or -1 for unlimited size.
   * @param maxConnections
   *   The maximum number of configured connections for the connection pool. The underlying ThreadPoolExecutor will not
   *   pick up any more work when all connections are in use. It will resume as soon as a connection is released again
   *   to the pool
   * @param registerMbeans
   *   If set to true, register an MXBean that provides insight into the current queue and thread pool workload.
   */
  def apply(
      name: String,
      minThreads: Int,
      maxThreads: Int,
      queueSize: Int,
      maxConnections: Int,
      registerMbeans: Boolean
  ): AsyncExecutor =
    apply(
      name,
      minThreads = minThreads,
      maxThreads = maxThreads,
      queueSize = queueSize,
      maxConnections = maxConnections,
      keepAliveTime = 1.minute,
      registerMbeans = registerMbeans
    )

  /**
   * Create an [[AsyncExecutor]] with a thread pool suitable for blocking I/O. New threads are created as daemon
   * threads.
   *
   * @param name
   *   A prefix to use for the names of the created threads.
   * @param minThreads
   *   The number of core threads in the pool.
   * @param maxThreads
   *   The maximum number of threads in the pool.
   * @param queueSize
   *   The size of the job queue, 0 for direct hand-off or -1 for unlimited size.
   * @param maxConnections
   *   The maximum number of configured connections for the connection pool. The underlying ThreadPoolExecutor will not
   *   pick up any more work when all connections are in use. It will resume as soon as a connection is released again
   *   to the pool
   * @param keepAliveTime
   *   when the number of threads is greater than the core, this is the maximum time that excess idle threads will wait
   *   for new tasks before terminating.
   * @param registerMbeans
   *   If set to true, register an MXBean that provides insight into the current queue and thread pool workload.
   */
  def apply(
      name: String,
      minThreads: Int,
      maxThreads: Int,
      queueSize: Int,
      maxConnections: Int,
      keepAliveTime: Duration,
      registerMbeans: Boolean
  ): AsyncExecutor = {
    class AsyncExecutorImpl(queue: BlockingQueue[Runnable])
        extends DefaultAsyncExecutor(
          name = name,
          minThreads = minThreads,
          maxThreads = maxThreads,
          queue = queue,
          queueSize = queueSize,
          maxConnections = maxConnections,
          keepAliveTime = keepAliveTime,
          registerMbeans = registerMbeans
        )

    queueSize match {
      case 0 =>
        // NOTE: SynchronousQueue does not schedule high-priority tasks before others and so it cannot be used when
        // the number of connections is limited (lest high-priority tasks may be holding all connections and low/mid
        // priority tasks all threads -- resulting in a deadlock).
        require(
          maxConnections == Int.MaxValue,
          "When using queueSize == 0 (direct hand-off), maxConnections must be Int.MaxValue."
        )

        new AsyncExecutorImpl(new SynchronousQueue[Runnable])

      case -1 =>
        // NOTE: LinkedBlockingQueue does not schedule high-priority tasks before others and so it cannot be used when
        // the number of connections is limited (lest high-priority tasks may be holding all connections and low/mid
        // priority tasks all threads -- resulting in a deadlock).
        require(
          maxConnections == Int.MaxValue,
          "When using queueSize == -1 (unlimited), maxConnections must be Int.MaxValue."
        )

        new AsyncExecutorImpl(new LinkedBlockingQueue[Runnable])

      case n =>
        // NOTE: The current implementation of ManagedArrayBlockingQueue is flawed. It makes the assumption that all
        // tasks go through the queue (which is responsible for scheduling high-priority tasks first). However, that
        // assumption is wrong since the ThreadPoolExecutor bypasses the queue when it creates new threads. This
        // happens whenever it creates a new thread to run a task, i.e. when minThreads < maxThreads and the number
        // of existing threads is < maxThreads.
        //
        // The only way to prevent problems is to have minThreads == maxThreads when using the
        // ManagedArrayBlockingQueue.
        require(minThreads == maxThreads, "When using queueSize > 0, minThreads == maxThreads is required.")

        // NOTE: The current implementation of ManagedArrayBlockingQueue.increaseInUseCount implicitly `require`s that
        // maxThreads <= maxConnections.
        require(maxThreads <= maxConnections, "When using queueSize > 0, maxThreads <= maxConnections is required.")

        // NOTE: Adding up the above rules
        // - maxThreads >= maxConnections, to prevent database locking issues when using transactions
        // - maxThreads <= maxConnections, required by ManagedArrayBlockingQueue
        // - maxThreads == minThreads, ManagedArrayBlockingQueue
        //
        // We have maxThreads == minThreads == maxConnections as the only working configuration

        val managedArrayBlockingQueue = new ManagedArrayBlockingQueue(maxConnections, n)

        new AsyncExecutorImpl(managedArrayBlockingQueue.asInstanceOf[BlockingQueue[Runnable]]) {
          override def prioritizedRunnable(
              priority0: => Priority,
              run0: PrioritizedRunnable.SetConnectionReleased => Unit
          ): PrioritizedRunnable =
            new PrioritizedRunnable {
              override def priority() = priority0

              private def runAndCleanUp() =
                try run0(new PrioritizedRunnable.SetConnectionReleased(() => connectionReleased = true))
                finally
                  // If the runnable/task has released the Jdbc connection we decrease the counter again
                  if (!managedArrayBlockingQueue.attemptCleanUp(this))
                    logger.warn("After executing a task, the in-use count was already 0. This should not happen.")

              override def run() =
                if (priority() == WithConnection)
                  runAndCleanUp()
                else if (managedArrayBlockingQueue.attemptPrepare(this))
                  runAndCleanUp()
                else {
                  logger.warn("Could not increase in-use count. Will resubmit...")
                  executor.execute(this)
                }
            }
        }
    }
  }

  def default(name: String, maxConnections: Int): AsyncExecutor =
    apply(
      name,
      minThreads = 20,
      maxThreads = 20,
      queueSize = 1000,
      maxConnections = maxConnections
    )

  def default(name: String = "AsyncExecutor.default"): AsyncExecutor =
    apply(name, 20, 1000)

  private[slick] class DefaultAsyncExecutor(
      name: String,
      minThreads: Int,
      maxThreads: Int,
      private[slick] val queue: BlockingQueue[Runnable],
      queueSize: Int,
      maxConnections: Int,
      keepAliveTime: Duration,
      registerMbeans: Boolean
  ) extends AsyncExecutor {

    @volatile private[this] lazy val mBeanName = new ObjectName(s"slick:type=AsyncExecutor,name=$name")

    // Before init: 0, during init: 1, after init: 2, during/after shutdown: 3
    private[this] val state = new AtomicInteger(0)

    @volatile protected var executor: ThreadPoolExecutor = _

    if (maxConnections > maxThreads) {
      // NOTE: when using transactions or DB locks, it may happen that a task has a lock on the database but no thread
      // to complete its action, while other tasks may have all the threads but are waiting for the first task to
      // complete. This creates a deadlock.
      logger.warn(
        "Having maxConnection > maxThreads can result in deadlocks if transactions or database locks are used."
      )
    }

    lazy val executionContext: ExecutionContext = {
      if (!state.compareAndSet(0, 1))
        throw new IllegalStateException("Cannot initialize ExecutionContext; AsyncExecutor already shut down")
      val tf = new DaemonThreadFactory(name + "-")
      executor =
        new ThreadPoolExecutor(minThreads, maxThreads, keepAliveTime.toMillis, TimeUnit.MILLISECONDS, queue, tf)

      if (registerMbeans) {
        try {
          val mBeanServer = ManagementFactory.getPlatformMBeanServer
          if (mBeanServer.isRegistered(mBeanName))
            logger.warn(s"MBean $mBeanName already registered (AsyncExecutor names should be unique)")
          else {
            logger.debug(s"Registering MBean $mBeanName")
            mBeanServer.registerMBean(
              new AsyncExecutorMXBean {
                def getMaxQueueSize = queueSize
                def getQueueSize = queue.size()
                def getMaxThreads = maxThreads
                def getActiveThreads = executor.getActiveCount
              },
              mBeanName
            )
          }
        } catch { case NonFatal(ex) => logger.error("Error registering MBean", ex) }
      }
      if (!state.compareAndSet(1, 2)) {
        unregisterMbeans()
        executor.shutdownNow()
        throw new IllegalStateException(
          "Cannot initialize ExecutionContext; AsyncExecutor shut down during initialization"
        )
      }

      new ExecutionContextExecutor {
        override def reportFailure(t: Throwable): Unit = loggingReporter(t)

        override def execute(command: Runnable): Unit =
          if (command.isInstanceOf[PrioritizedRunnable]) {
            executor.execute(command)
          } else {
            executor.execute(new PrioritizedRunnable {
              override def priority(): Priority = WithConnection
              override def run(): Unit = command.run()
            })
          }
      }
    }

    private[this] def unregisterMbeans(): Unit = if (registerMbeans) {
      try {
        val mBeanServer = ManagementFactory.getPlatformMBeanServer
        logger.debug(s"Unregistering MBean $mBeanName")
        try mBeanServer.unregisterMBean(mBeanName)
        catch { case _: InstanceNotFoundException => }
      } catch { case NonFatal(ex) => logger.error("Error unregistering MBean", ex) }
    }

    def close(): Unit = if (state.getAndSet(3) == 2) {
      unregisterMbeans()
      executor.shutdownNow()
      if (!executor.awaitTermination(30, TimeUnit.SECONDS))
        logger.warn("Abandoning ThreadPoolExecutor (not yet destroyed after 30 seconds)")
    }
  }

  sealed trait Priority

  /** Fresh is used for database actions that are scheduled/queued for the first time. */
  case object Fresh extends Priority

  /** Continuation is used for database actions that are a continuation of some previously executed actions */
  case object Continuation extends Priority

  /** WithConnection is used for database actions that already have a JDBC connection associated. */
  case object WithConnection extends Priority

  sealed trait PrioritizedRunnable extends Runnable {
    def priority(): Priority

    /** true if the JDBC connection was released */
    var connectionReleased = false

    /** true if the inUseCounter of the ManagedArrayBlockQueue was incremented */
    var inUseCounterSet = false
  }
  object PrioritizedRunnable {
    class SetConnectionReleased(val f: () => Unit) extends AnyVal {
      def apply() = f()
    }

    def apply(priority: => Priority, run: SetConnectionReleased => Unit): PrioritizedRunnable = {
      def _priority = priority
      def _run(setConnectionReleased: SetConnectionReleased) = run(setConnectionReleased)
      new PrioritizedRunnable {
        def priority(): Priority = _priority
        def run(): Unit = _run(new SetConnectionReleased(() => connectionReleased = true))
      }
    }
  }

  private class DaemonThreadFactory(namePrefix: String) extends ThreadFactory {
    private[this] val group =
      Option(System.getSecurityManager).fold(Thread.currentThread.getThreadGroup)(_.getThreadGroup)
    private[this] val threadNumber = new AtomicInteger(1)

    def newThread(r: Runnable): Thread = {
      val t = new Thread(group, r, namePrefix + threadNumber.getAndIncrement, 0)
      if (!t.isDaemon) t.setDaemon(true)
      if (t.getPriority != Thread.NORM_PRIORITY) t.setPriority(Thread.NORM_PRIORITY)
      t
    }
  }

  /**
   * An Executor which spawns a new daemon thread for each command. It is useful for wrapping synchronous `close` calls
   * for asynchronous `shutdown` operations.
   */
  private[slick] val shutdownExecutor: Executor = { (command: Runnable) =>
    val t = new Thread(command)
    t.setName("shutdownExecutor")
    t.setDaemon(true)
    t.start()
  }

  private val loggingReporter: Throwable => Unit = (t: Throwable) =>
    logger.warn("Execution of asynchronous I/O action failed", t)
}

/** The information that is exposed by an [[AsyncExecutor]] via JMX. */
trait AsyncExecutorMXBean {

  /** Get the configured maximum queue size (0 for direct hand-off, -1 for unlimited) */
  def getMaxQueueSize: Int

  /** Get the current number of DBIOActions in the queue (waiting to be executed) */
  def getQueueSize: Int

  /** Get the configured maximum number of database I/O threads */
  def getMaxThreads: Int

  /** Get the number of database I/O threads that are currently executing a task */
  def getActiveThreads: Int
}
