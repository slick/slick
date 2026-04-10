package slick.basic

import cats.effect.kernel.Deferred
import cats.effect.std.Semaphore
import cats.effect.{Async, Ref}
import cats.syntax.all.*
import cats.effect.syntax.all.*

import scala.collection.immutable.TreeMap

import slick.SlickException

object ConcurrencyControl {
  final case class Controls[F[_]](
    admissionControl: AdmissionControl[F],
    connectionArbiter: ConnectionArbiter[F]
  )

  object Controls {
    def create[F[_]](maxConnections: Long, queueSize: Long, maxInflight: Long)(implicit F: Async[F]): F[Controls[F]] = {
      require(maxConnections > 0, s"maxConnections must be > 0, got $maxConnections")
      require(queueSize >= 0, s"queueSize must be >= 0, got $queueSize")
      require(maxInflight > 0, s"maxInflight must be > 0, got $maxInflight")
      F.flatMap(Semaphore[F](queueSize)) { queueSem =>
        F.flatMap(Semaphore[F](maxInflight)) { inflightSem =>
          F.map(ConnectionArbiter.create[F](maxConnections)) { arbiter =>
            Controls(new AdmissionControl[F](queueSem, inflightSem), arbiter)
          }
        }
      }
    }
  }

  final class AdmissionControl[F[_]](
    queue: Semaphore[F],
    inflight: Semaphore[F]
  )(implicit private val F: Async[F]) {
    // Run helper

    /** Admit a non-streaming run and manage inflight release internally.
      * Callers must not call `releaseInflight` themselves for this path. */
    def withInflight[R](fr: => F[R]): F[R] =
      queue.tryAcquire.flatMap {
        case false =>
          F.raiseError(new SlickException("DBIOAction queue full"))
        case true =>
          F.uncancelable { poll =>
            poll(inflight.acquire)
              .onCancel(queue.release) >>
            queue.release >>
            poll(fr).guarantee(inflight.release)
          }
      }

    // Streaming helpers

    /** Admit a streaming run and acquire inflight only.
      * Callers must pair a successful `inflightAcquire` with `inflightRelease`. */
    def inflightAcquire: F[Unit] =
      queue.tryAcquire.flatMap {
        case false =>
          F.raiseError(new SlickException("DBIOAction queue full"))
        case true =>
          F.uncancelable { poll =>
            poll(inflight.acquire)
              .onCancel(queue.release) >>
            queue.release
          }
      }

    /** Release one inflight permit. Use this as the paired operation after
      * successful `inflightAcquire` (e.g. stream finalization). */
    def inflightRelease: F[Unit] =
      inflight.release

    def queueAvailable: F[Long] =
      queue.available

    def inflightAvailable: F[Long] =
      inflight.available
  }

  final class ConnectionArbiter[F[_]] private (
    state: Ref[F, ConnectionArbiter.State[F]]
  )(implicit private val F: Async[F]) {
    import ConnectionArbiter.*

    def available: F[Long] =
      state.get.map(_.available)

    def pending: F[Int] =
      state.get.map(_.waiting.size)

    /** Allocate a globally-ordered ordinal for one submitted DBIO chain. */
    def allocateOrdinal: F[Long] =
      state.modify { s =>
        val n = s.nextOrdinal
        (s.copy(nextOrdinal = n + 1L), n)
      }

    def acquire(ordinal: Long): F[Unit] =
      F.uncancelable { poll =>
        Deferred[F, Unit].flatMap { gate =>
          state.modify[Option[WaiterKey]] { s =>
            if ((s.available > 0 && s.waiting.nonEmpty) || s.available < 0) {
              throw new IllegalStateException(
                s"ConnectionArbiter invariant violated in acquire: available=${s.available}, waiters=${s.waiting.size}"
              )
            } else if (s.available == 0) {
              // Queue case: no free slots
              if (s.waiting.contains(ordinal))
                throw new IllegalStateException(s"Duplicate ordinal enqueued in ConnectionArbiter: $ordinal")
              (s.copy(waiting = s.waiting.updated(ordinal, gate)), Some(ordinal))
            } else {
              assert(s.available > 0 && s.waiting.isEmpty)
              // Run case: free slot and no queued waiters
              (s.copy(available = s.available - 1), None)
            }
          }.flatMap {
            case None => F.unit
            case Some(key) => poll(gate.get).onCancel(cancelWaiter(key))
          }
        }
      }

    def release: F[Unit] =
      state.modify[Option[Deferred[F, Unit]]] { s =>
        s.waiting.headOption match {
          case Some((key, gate)) =>
            (s.copy(waiting = s.waiting - key), Some(gate))
          case None =>
            (s.copy(available = s.available + 1), None)
        }
      }.flatMap {
        case Some(gate) => gate.complete(()).void
        case None => F.unit
      }

    private def cancelWaiter(key: WaiterKey): F[Unit] =
      state.update(s => s.copy(waiting = s.waiting - key))
  }

  object ConnectionArbiter {
    type WaiterKey = Long

    final case class State[F[_]](
      available: Long,
      nextOrdinal: Long,
      waiting: TreeMap[WaiterKey, Deferred[F, Unit]]
    )

    def create[F[_]](maxConnections: Long)(implicit F: Async[F]): F[ConnectionArbiter[F]] = {
      require(maxConnections > 0, s"maxConnections must be > 0, got $maxConnections")
      Ref.of[F, State[F]](State(maxConnections, Long.MinValue, TreeMap.empty)).map(new ConnectionArbiter(_)(F))
    }
  }
}
