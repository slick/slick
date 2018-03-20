package slick.util

import java.io.Closeable
import java.lang.management.ManagementFactory
import java.util.concurrent._
import javax.management.{InstanceNotFoundException, ObjectName}

import scala.concurrent.duration._
import scala.concurrent._
import java.util.concurrent.atomic.AtomicInteger

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
  def apply(name: String, numThreads: Int, queueSize: Int): AsyncExecutor = apply(name, numThreads, numThreads, queueSize)

  /** Create an [[AsyncExecutor]] with a thread pool suitable for blocking
    * I/O. New threads are created as daemon threads.
    *
    * @param name A prefix to use for the names of the created threads.
    * @param minThreads The number of core threads in the pool.
    * @param maxThreads The maximum number of threads in the pool.
    * @param queueSize The size of the job queue, 0 for direct hand-off or -1 for unlimited size.
    * @param maxConnections The maximum number of configured connections for the connection pool.
    *                       The underlying ThreadPoolExecutor will not pick up any more work when all connections are in use.
    *                       It will resume as soon as a connection is released again to the pool
    *                       Default is Integer.MAX_VALUE which is only ever a good choice when not using connection pooling
    * @param keepAliveTime when the number of threads is greater than
    *        the core, this is the maximum time that excess idle threads
    *        will wait for new tasks before terminating.
    * @param registerMbeans If set to true, register an MXBean that provides insight into the current
    *        queue and thread pool workload. */
  def apply(name: String, minThreads: Int, maxThreads: Int, queueSize: Int, maxConnections: Int = Integer.MAX_VALUE, keepAliveTime: Duration = 1.minute,
            registerMbeans: Boolean = false): AsyncExecutor = new AsyncExecutor {
    
    @volatile private[this] lazy val mbeanName = new ObjectName(s"slick:type=AsyncExecutor,name=$name");

    // Before init: 0, during init: 1, after init: 2, during/after shutdown: 3
    private[this] val state = new AtomicInteger(0)

    @volatile private[this] var executor: ThreadPoolExecutor = _

    if (maxConnections > maxThreads) {
      // NOTE: when using transactions or DB locks, it may happen that a task has a lock on the database but no thread
      // to complete its action, while other tasks may have all the threads but are waiting for the first task to
      // complete. This creates a deadlock.
      logger.warn("Having maxConnection > maxThreads can result in deadlocks if transactions or database locks are used.")
    }

    lazy val executionContext = {
      if(!state.compareAndSet(0, 1))
        throw new IllegalStateException("Cannot initialize ExecutionContext; AsyncExecutor already shut down")
      val queue: BlockingQueue[Runnable] = queueSize match {
        case 0 =>
          // NOTE: SynchronousQueue does not schedule high-priority tasks before others and so it cannot be used when
          // the number of connections is limited (lest high-priority tasks may be holding all connections and low/mid
          // priority tasks all threads -- resulting in a deadlock).
          require(maxConnections == Integer.MAX_VALUE, "When using queueSize == 0 (direct hand-off), maxConnections must be Integer.MAX_VALUE.")

          new SynchronousQueue[Runnable]
        case -1 =>
          // NOTE: LinkedBlockingQueue does not schedule high-priority tasks before others and so it cannot be used when
          // the number of connections is limited (lest high-priority tasks may be holding all connections and low/mid
          // priority tasks all threads -- resulting in a deadlock).
          require(maxConnections == Integer.MAX_VALUE, "When using queueSize == -1 (unlimited), maxConnections must be Integer.MAX_VALUE.")

          new LinkedBlockingQueue[Runnable]
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

          new ManagedArrayBlockingQueue(maxConnections, n).asInstanceOf[BlockingQueue[Runnable]]
      }
      val tf = new DaemonThreadFactory(name + "-")
      executor = new ThreadPoolExecutor(minThreads, maxThreads, keepAliveTime.toMillis, TimeUnit.MILLISECONDS, queue, tf) {

        /** If the runnable/task is a low/medium priority item, we increase the items in use count, because first thing it will do
          * is open a Jdbc connection from the pool.
          */
        override def beforeExecute(t: Thread, r: Runnable): Unit = {
          (r, queue) match {
            case (pr: PrioritizedRunnable, q: ManagedArrayBlockingQueue[Runnable]) if pr.priority != WithConnection => q.increaseInUseCount(pr)
            case _ =>
          }
          super.beforeExecute(t, r)
        }

        /**
          * If the runnable/task has released the Jdbc connection we decrease the counter again
          */
        override def afterExecute(r: Runnable, t: Throwable): Unit = {
          super.afterExecute(r, t)
          (r, queue) match {
            case (pr: PrioritizedRunnable, q: ManagedArrayBlockingQueue[Runnable]) if pr.connectionReleased => q.decreaseInUseCount()
            case _ =>
          }
        }

      }
      if(registerMbeans) {
        try {
          val mbeanServer = ManagementFactory.getPlatformMBeanServer
          if(mbeanServer.isRegistered(mbeanName))
            logger.warn(s"MBean $mbeanName already registered (AsyncExecutor names should be unique)")
          else {
            logger.debug(s"Registering MBean $mbeanName")
            mbeanServer.registerMBean(new AsyncExecutorMXBean {
              def getMaxQueueSize = queueSize
              def getQueueSize = queue.size()
              def getMaxThreads = maxThreads
              def getActiveThreads = executor.getActiveCount
            }, mbeanName)
          }
        } catch { case NonFatal(ex) => logger.error("Error registering MBean", ex) }
      }
      if(!state.compareAndSet(1, 2)) {
        unregisterMbeans()
        executor.shutdownNow()
        throw new IllegalStateException("Cannot initialize ExecutionContext; AsyncExecutor shut down during initialization")
      }
      new ExecutionContextExecutor {
        override def reportFailure(t: Throwable): Unit = loggingReporter(t)

        override def execute(command: Runnable): Unit = {
          if (command.isInstanceOf[PrioritizedRunnable]) {
            executor.execute(command)
          } else {
            executor.execute(new PrioritizedRunnable {
              override val priority: Priority = WithConnection
              override def run(): Unit = command.run()
            })
          }
        }
      }
    }

    private[this] def unregisterMbeans(): Unit = if(registerMbeans) {
      try {
        val mbeanServer = ManagementFactory.getPlatformMBeanServer
        logger.debug(s"Unregistering MBean $mbeanName")
        try mbeanServer.unregisterMBean(mbeanName) catch { case _: InstanceNotFoundException => }
      } catch { case NonFatal(ex) => logger.error("Error unregistering MBean", ex) }
    }

    def close(): Unit = if(state.getAndSet(3) == 2) {
      unregisterMbeans()
      executor.shutdownNow()
      if(!executor.awaitTermination(30, TimeUnit.SECONDS))
        logger.warn("Abandoning ThreadPoolExecutor (not yet destroyed after 30 seconds)")
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

  sealed trait Priority
  /** Fresh is used for database actions that are scheduled/queued for the first time. */
  case object Fresh extends Priority
  /** Continuation is used for database actions that are a continuation of some previously executed actions */
  case object Continuation extends Priority
  /** WithConnection is used for database actions that already have a JDBC connection associated. */
  case object WithConnection extends Priority

  trait PrioritizedRunnable extends Runnable {
    def priority: Priority
    /** true if the JDBC connection was released */
    var connectionReleased = false
    /** true if the inUseCounter of the ManagedArrayBlockQueue was incremented */
    var inUseCounterSet = false
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
