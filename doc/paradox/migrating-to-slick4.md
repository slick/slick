Migrating to Slick 4
====================

@@@ note
This guide is written for both human readers and AI agents performing automated migrations. The
[Mechanical Migration Checklist](#mechanical-migration-checklist) at the end contains a concise,
pattern-by-pattern reference suitable for automated tooling.

For upgrade notes between Slick 3.x minor versions, see @ref:[Slick 3.x Upgrade Notes](upgrade-slick3.md).
@@@

Slick 4 replaces the concurrency and execution layer with
@extref[Cats Effect 3](cats-effect:) and @extref[FS2](fs2:). The query DSL, SQL compiler pipeline,
`DBIOAction` type and phantom effect system (`Effect.Read`/`Write`/`Schema`/`Transactional`), all
database profiles, and all SQL generation code are **completely unchanged**. If your application
only composes queries and runs them, the migration is largely mechanical.

## What Changed and What Did Not

### Unchanged

- All query syntax: `users.filter(_.id === 1).result`, `users += row`, `.delete`, `.update`,
  `schema.create`, `schema.createIfNotExists`, plain SQL interpolation (`sql"..."`, `sqlu"..."`)
- The `DBIOAction[R, S, E]` type and all its combinators: `andThen`, `zip`, `DBIO.seq`,
  `DBIO.sequence`, `DBIO.traverse`, `DBIO.fold`, `asTry`, `failed`, `andFinally`, `cleanUp`,
  `withPinnedSession`, `transactionally`, `named`
- All profile imports: `import slick.jdbc.PostgresProfile.api.*`
- The phantom effect type system: `Effect.Read`, `Effect.Write`, `Effect.Schema`,
  `Effect.Transactional`
- `slick-hikaricp`: HikariCP integration is unchanged; `DataSource` is still the integration point
- `slick-codegen`: code generation is unchanged. If you have previously generated code that
  contains `import scala.concurrent.ExecutionContext` (added by older codegen versions), that
  import is now unused — remove it or regenerate.
- `DatabaseConfig.forConfig[P, F](path)` — still works; see [DatabaseConfig](#databaseconfig) below

### Changed (breaking)

| Area | Before (Slick 3) | After (Slick 4) |
|------|-----------------|-----------------|
| `db.run(a)` return type | `Future[R]` | `F[R]` (e.g. `IO[R]`) |
| `db.stream(a)` return type | `DatabasePublisher[T]` | `Stream[F, T]` |
| `Database` construction | `Database.forConfig("p")` returns `Database` | `Database.forConfig[IO]("p")` returns `Resource[IO, Database[IO]]` |
| `map`/`flatMap`/`filter`/`cleanUp`/`zipWith`/`DBIO.fold` | require `(implicit ec: ExecutionContext)` | no EC parameter |
| `DBIO.from(x)` | lifts a `Future[R]` | lifts any `F[R]` (CE3 effect) |
| `DBIO.liftF` | did not exist | alias for `DBIO.from` |
| `AsyncExecutor` | required for thread pool config | **removed** entirely |
| `db.shutdown` | `Future[Unit]` | **removed** — use `Resource` finalizer or `db.close()` |
| `db.close()` (manual) | `Unit` | still works; `Resource` finalizer is preferred — see [Database Construction](#2-database-construction) |
| `ioExecutionContext` | `ExecutionContext` for blocking work | **removed** — use `db.io(thunk)` which returns `F[T]` |

---

## 1. Dependencies

### Add Cats Effect 3 and FS2

```scala
libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-effect" % "3.6.x",
  "co.fs2"        %% "fs2-core"    % "3.x"
)
```

### Scala version

Slick 4 supports **Scala 2.12**, **Scala 2.13**, and **Scala 3** (primary).

### Reactive Streams (optional)

The `reactive-streams` artifact is no longer a core dependency. If you need to expose a
`Publisher[T]` for interop with Akka Streams or another Reactive Streams consumer, add the
optional compatibility module:

```scala
libraryDependencies += "com.typesafe.slick" %% "slick-reactive-streams" % "$project.version$"
```

See [Reactive Streams Compatibility](#reactive-streams-compatibility) for usage.

---

## 2. Database Construction

All factory methods now return `Resource[F, Database[F]]`, where `F[_]` is your effect type
(e.g. `cats.effect.IO`). The effect type is fixed at construction time — pass it as a type
argument.

There are three ways to manage the database lifecycle, listed here **in order of preference**:

### Option 1 — `Resource.use` (recommended)

The `Resource` finalizer closes the connection pool automatically when the `use` block completes,
fails, or is cancelled. No manual `close()` call is needed.

```scala
// Slick 3
val db = Database.forConfig("mydb")
try {
  Await.result(program(db), Duration.Inf)
} finally {
  Await.result(db.shutdown, Duration.Inf)
}

// Slick 4
Database.forConfig[IO]("mydb").use { db =>
  program(db)
}
```

This is the cleanest approach and the one to aim for in new code.

### Option 2 — `Resource.allocated` with explicit `closeDb`

If your code cannot easily be restructured around a single `use` block — for example when `db` is
stored in a DI container or passed through a framework lifecycle — use `Resource.allocated`. It
returns an `F[(Database[F], F[Unit])]` where the second element is the finalizer you call on
shutdown:

```scala
val (db, closeDb) = Database.forConfig[IO]("mydb").allocated.unsafeRunSync()

program(db)

// On shutdown — idiomatic: compose into your shutdown logic
closeDb.unsafeRunSync()
```

This is the right migration step when restructuring around `Resource.use` is too large a change
for one PR.

### Option 3 — `Resource.allocated`, discard finalizer, call `db.close()` directly

For the minimal-change migration — old code that already calls `db.close()` in a shutdown hook
— you can discard the `closeDb` finalizer entirely and keep calling `db.close()` as before:

```scala
val (db, _) = Database.forConfig[IO]("mydb").allocated.unsafeRunSync()

program(db)

db.close()  // exactly as in Slick 3
```

This is safe because `db.close()` and the `Resource` finalizer call the same underlying
`HikariDataSource.close()`, which is idempotent. Calling `db.close()` directly is equivalent to
calling `closeDb.unsafeRunSync()`. The only thing you give up is the ability to compose shutdown
into your effect chain.

Use this option as a **temporary stepping stone** — it lets you change nothing else in your
codebase while you migrate incrementally toward Option 1 or 2.

### All factory methods

All factory methods return `Resource[F, Database[F]]`:

| Method | Slick 4 signature |
|--------|------------------|
| `Database.forConfig[F](path)` | `Resource[F, Database[F]]` |
| `Database.forURL[F](url, ...)` | `Resource[F, Database[F]]` |
| `Database.forDataSource[F](ds, maxConn)` | `Resource[F, Database[F]]` |
| `Database.forName[F](jndi, maxConn)` | `Resource[F, Database[F]]` |

For **library or framework code** that must remain polymorphic over the effect type:

```scala
def makeDb[F[_]: Async]: Resource[F, Database[F]] =
  Database.forConfig[F]("mydb")
```

### DatabaseConfig

`DatabaseConfig.forConfig` is also effect-aware in Slick 4. Use the two-type-parameter overload:

```scala
// Slick 4
DatabaseConfig.forConfig[JdbcProfile, IO]("mydb").use { dc =>
  val db: Database[IO] = dc.db
  import dc.profile.api.*
  db.run(users.result)
}
```

---

## 3. `db.run` — running actions

`db.run` now returns `F[R]` directly. Compose it with the rest of your CE3 program:

```scala
// Slick 3
val future: Future[Seq[User]] = db.run(users.result)
val result: Seq[User] = Await.result(future, 10.seconds)

// Slick 4
val io: IO[Seq[User]] = db.run(users.result)
// compose with other IO values normally:
io.flatMap(rows => IO(rows.foreach(println)))
```

No `Await.result`, no `scala.concurrent.ExecutionContext` needed anywhere.

---

## 4. `db.stream` — streaming results

`db.stream` returns an FS2 `Stream[F, T]` instead of a Reactive Streams `DatabasePublisher[T]`.

```scala
// Slick 3
val publisher: DatabasePublisher[User] = db.stream(users.result)
publisher.foreach(u => println(u))  // returns Future[Unit]

// Slick 4
val stream: Stream[IO, User] = db.stream(users.result)
stream.evalMap(u => IO(println(u))).compile.drain  // IO[Unit]
```

Common FS2 patterns for consuming a stream:

```scala
// Collect all results into a Vector
db.stream(users.result).compile.toVector          // IO[Vector[User]]

// Take the first N
db.stream(users.result).take(100).compile.toVector

// Process each element effectfully
db.stream(users.result).evalMap(process).compile.drain

// Fold to a summary value
db.stream(q.result).compile.fold(0)((n, _) => n + 1)
```

Streaming with transactions works identically to Slick 3 — just wrap with `.transactionally`:

```scala
db.stream(users.result.transactionally)
```

@@@ note
PostgreSQL requires both `.withStatementParameters(rsType = ResultSetType.ForwardOnly,
rsConcurrency = ResultSetConcurrency.ReadOnly, fetchSize = n)` and `.transactionally` for
proper server-side cursor streaming (same requirement as before).
@@@

---

## 5. `ExecutionContext` removal

`map`, `flatMap`, `filter`, `withFilter`, `cleanUp`, `zipWith`, and `DBIO.fold` **no longer accept
an implicit `ExecutionContext`**. Remove all EC imports and implicits that were only present for
DBIO composition.

```scala
// Slick 3 — required import and implicit
import scala.concurrent.ExecutionContext.Implicits.global

val action: DBIO[String] = for {
  count <- users.length.result
  _     <- users += User(count + 1, "Alice")
} yield "done"

// Slick 4 — no import or implicit needed
val action: DBIO[String] = for {
  count <- users.length.result
  _     <- users += User(count + 1, "Alice")
} yield "done"
```

### Play Framework

Play injects an `ExecutionContext` into controllers and services via DI. In Slick 4 you no
longer pass it to DBIO combinators, but you may still need it for other `Future`-based code in
the same class. Bridge `IO[R]` to `Future[R]` at the outermost point:

```scala
// Slick 4 with Play
import cats.effect.IO
import cats.effect.unsafe.implicits.runtime   // or inject IORuntime

class UserController @Inject()(db: Database[IO], cc: ControllerComponents)
    extends AbstractController(cc) {

  def list = Action.async {
    db.run(users.result).unsafeToFuture()     // IO[Seq[User]] → Future[Seq[User]]
      .map(rows => Ok(Json.toJson(rows)))(cc.executionContext)
  }
}
```

### Pekko / Akka

Pekko and Akka actors expose `context.dispatcher: ExecutionContext`. In Slick 4, remove it
from DBIO combinators. It is still available for `pipeTo`, `ask`, and other
`Future` operations.

```scala
// Slick 4 with Pekko
import cats.effect.IO
import cats.effect.unsafe.implicits.runtime   // or use a custom IORuntime

class UserActor(db: Database[IO]) extends Actor {
  import context.dispatcher   // still needed for pipeTo / ask, not for DBIO

  def receive = {
    case GetUsers =>
      db.run(users.result)
        .unsafeToFuture()         // IO[Seq[User]] → Future[Seq[User]]
        .pipeTo(sender())
  }
}
```

Note: In Slick 3, every `Database` instance managed its own internal thread pool (sized to the
connection pool) alongside whatever pool the surrounding framework was using. In Slick 4 that
internal pool is gone — multiple `Database` instances can share a single `IORuntime`.

@@@ warning
Do not use Play's or Pekko's dispatcher as the CE3 compute pool via `IORuntime.apply`. CE3's
work-stealing pool is required for `IO.blocking` to correctly spawn a replacement thread before
each JDBC call blocks; using a foreign dispatcher disables this mechanism and can starve the
compute pool under load.
@@@

### `DBIO.fold`

`DBIO.fold` no longer takes an `ExecutionContext`:

```scala
// Slick 3
DBIO.fold(actions, zero)(f)(ec)

// Slick 4
DBIO.fold(actions, zero)(f)
```

---

## 6. `DBIO.from` and `DBIO.liftF`

`DBIO.from` now lifts a CE3 effect `F[R]` into a `DBIOAction`, not a `Future`.
`DBIO.liftF` is an alias for `DBIO.from`.

```scala
// Slick 3
val action = DBIO.from(Future(computeSomething()))

// Slick 4
val action = DBIO.from(IO(computeSomething()))
// or equivalently:
val action = DBIO.liftF(IO(computeSomething()))
```

If you have an existing `Future` you need to bridge:

```scala
val action = DBIO.from(IO.fromFuture(IO(myExistingFuture)))
```

A typical pattern — generating a value using an effect before inserting:

```scala
val insertWithGeneratedId: DBIO[Unit] = for {
  id <- DBIO.from(IO(java.util.UUID.randomUUID().toString))
  _  <- users += User(id, "Alice")
} yield ()
```

---

## 7. Transaction API

### `.transactionally` — unchanged

`.transactionally` works identically to before. The important new guarantee is that in Slick 4
the transaction is **rolled back on fiber cancellation**, not just on errors. This was not
possible with `Future`-based Slick 3.

```scala
val transfer: DBIO[Unit] = (for {
  _ <- accounts.filter(_.id === from).map(_.balance).update(fromBal - amount)
  _ <- accounts.filter(_.id === to).map(_.balance).update(toBal + amount)
} yield ()).transactionally

db.run(transfer)  // IO[Unit] — rolled back on error OR cancellation
```

`transactionally` has an overload that accepts a `TransactionIsolation` level:

```scala
action.transactionally(TransactionIsolation.Serializable)
```

---

## 8. Savepoints

Savepoint APIs are not available in this branch, so there is nothing to migrate here yet.

---

## 9. `db.io` — running blocking work in `F`

If you need to run a blocking JDBC-level call outside of `SimpleDBIO`, use `db.io`:

```scala
// Slick 4
val result: IO[Boolean] = db.io(conn.getAutoCommit)
```

This is the replacement for `db.ioExecutionContext` (which has been removed). `db.io` returns
`F[T]`, shifting the thunk to the CE3 blocking thread pool.

---

## 10. Reactive Streams Compatibility

If you need a `Publisher[T]` for interop with Akka Streams or another Reactive Streams consumer,
use the optional `slick-reactive-streams` module:

```scala
import slick.reactivestreams.*

// Requires a cats.effect.std.Dispatcher[F] from your app context
val publisherResource: Resource[IO, Publisher[User]] =
  Database.forConfig[IO]("mydb").flatMap { db =>
    db.stream(users.result).toUnicastPublisher
  }
```

The core `slick` artifact no longer depends on `reactive-streams`.

---

## 11. Completely Removed APIs

The following APIs are removed and have **no direct replacement** in Slick 4 (the need they
addressed is either gone or covered by a different mechanism):

### `AsyncExecutor`

`AsyncExecutor`, `ManagedArrayBlockingQueue`, and `InternalArrayQueue` are deleted. CE3 manages
all threading internally. The `minThreads == maxThreads == maxConnections` constraint that was
required to prevent deadlocks in Slick 3 **does not exist in Slick 4**.

No code changes needed other than removing any `AsyncExecutor` construction. Do not pass it to
factory methods — none of the Slick 4 factory methods accept it.

If you previously sized your HikariCP pool conservatively to satisfy the Slick 3 deadlock constraint
(`maximumPoolSize == threadCount`), you are now free to size it independently of thread counts —
HikariCP pool size can be set to whatever your database server supports.

### `db.shutdown` and `db.close()`

`shutdown: Future[Unit]` is removed entirely.

`close(): Unit` is still implemented on `Database` (it is called by the `Resource` finalizer), and
calling it directly still works — see [Option 3](#option-3----resource-allocated-discard-finalizer-call-dbclose-directly)
above. However, the preferred migration is to wrap your program in `Resource.use` and let the
finalizer handle cleanup, or to use `Resource.allocated` and keep the `closeDb` effect:

```scala
// Slick 3
val db = Database.forConfig("mydb")
try program(db).onComplete(_ => db.close())
finally Await.result(db.shutdown, Duration.Inf)

// Slick 4 — preferred
Database.forConfig[IO]("mydb").use(db => program(db))
```

### `db.stream(action, bufferNext)` overload

The two-argument `stream(action, bufferNext: Boolean)` overload is removed. In Slick 3,
`bufferNext = false` kept the result set open while an async consumer processed each element,
which was required to keep LOB references (e.g. `java.sql.Blob`) valid on databases such as DB2.

In Slick 4 the FS2 pull model is inherently demand-driven: the next row is only fetched when
downstream requests it, giving the same guarantee unconditionally. Use `db.stream(action)` — no
flag is needed or accepted.

```scala
// Slick 3
db.stream(blobQuery.result, bufferNext = false)

// Slick 4
db.stream(blobQuery.result)   // demand-driven by default
```

### `DatabasePublisher[T]` (core)

`DatabasePublisher[T]` is removed from the `slick` core module. Use `db.stream(...)` which
returns `Stream[F, T]` (FS2) instead. If you need `Publisher[T]` interop, use the optional
`slick-reactive-streams` module (see [above](#reactive-streams-compatibility)).

### `DBIO.sameThreadExecutionContext`

Removed. It was an internal trampoline used to prevent stack overflows in deep action chains.
CE3's fiber runloop is stack-safe by design; no replacement is needed.

### JMX `AsyncExecutorMXBean` (`slick:type=AsyncExecutor,name=…`)

The `AsyncExecutorMXBean` JMX bean is removed together with `AsyncExecutor`. HikariCP's own JMX
beans (`com.zaxxer.hikari:type=Pool` and `com.zaxxer.hikari:type=PoolConfig`) still work
unchanged. For CE3 runtime metrics (fiber counts, thread pool utilization), use a CE3-compatible
metrics integration such as `metrics4cats`.

### `ioExecutionContext: ExecutionContext`

`db.ioExecutionContext` is removed. CE3's blocking pool is managed internally by the runtime and
has no single exposed `ExecutionContext`. Use `db.io(thunk)` which returns `F[T]` for running
blocking JDBC work, or `Async[F].blocking(thunk)` directly.

### `unsafeBeginTransaction`, `unsafeCommitTransaction`, `unsafeRollbackTransaction`, `isInTransaction`

These were added just before the Slick 4 boundary and never shipped in a release. They are not
present in Slick 4. Use `.transactionally` to scope transactions in DBIO composition.

---

## 12. ZIO Interop

Slick 4 works with ZIO out of the box via
[zio-interop-cats](https://github.com/zio/zio-interop-cats), which provides a `cats.effect.Async`
instance for ZIO's `Task` type.

```scala
import zio.interop.catz.*

// Database[Task] — fully ZIO-native
val dbResource: Resource[Task, Database[Task]] = Database.forConfig[Task]("mydb")

dbResource.use { db =>
  db.run(users.result)   // Task[Seq[User]]
}
```

If you were previously using `ZIO.fromFuture(_ => db.run(dbio))`, replace it with
`db.run(dbio)` directly (returning `Task[R]`).

@@@ warning
The third-party `zio-slick-interop` library is **not compatible with Slick 4** and needs to be
rewritten for the new API. If you depend on it, migrate to using `db.run(dbio)` directly via
`zio-interop-cats` as shown above.
@@@

---

## Mechanical Migration Checklist

This section is a concise, pattern-by-pattern reference for automated migration tooling or for
developers doing a systematic find-and-replace pass.

### 1. Dependencies — `build.sbt` / `pom.xml`

- **Add**: `"org.typelevel" %% "cats-effect" % "3.x"`
- **Add**: `"co.fs2" %% "fs2-core" % "3.x"`
- **Remove** (or move to `slick-reactive-streams`): any direct dependency on
  `org.reactivestreams:reactive-streams`
- **Keep** Scala 2.12, 2.13, and Scala 3 cross-build entries

### 2. Imports to remove

Remove any import that was only present to provide an `ExecutionContext` for DBIO composition:

```
import scala.concurrent.ExecutionContext
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.ExecutionContext.parasitic
```

Keep any `ExecutionContext` imports that are used for non-Slick `Future` code in the same file.

### 3. `Database` construction

| Pattern (Slick 3) | Replacement (Slick 4) |
|-------------------|-----------------------|
| `Database.forConfig("p")` | `Database.forConfig[IO]("p")` → `Resource[IO, Database[IO]]` |
| `Database.forURL(url, driver=d)` | `Database.forURL[IO](url, driver=d)` → `Resource[IO, Database[IO]]` |
| `Database.forDataSource(ds, Some(n))` | `Database.forDataSource[IO](ds, Some(n))` → `Resource[IO, Database[IO]]` |
| `Database.forName(jndi, Some(n))` | `Database.forName[IO](jndi, Some(n))` → `Resource[IO, Database[IO]]` |
| `DatabaseConfig.forConfig[P]("p")` | `DatabaseConfig.forConfig[P, IO]("p")` → `Resource[IO, LoadedDatabaseConfig[P, IO]]` |

### 4. Database lifecycle

In order of preference:

| Pattern (Slick 3) | Replacement (Slick 4) | Option |
|-------------------|-----------------------|--------|
| `val db = Database.forConfig("p")` + `db.shutdown` / `db.close()` | `Database.forConfig[IO]("p").use { db => program(db) }` | 1 — preferred |
| `val db = Database.forConfig("p")` + `db.close()` on shutdown | `val (db, closeDb) = Database.forConfig[IO]("p").allocated.unsafeRunSync()` + `closeDb.unsafeRunSync()` on shutdown | 2 — migration step |
| `val db = Database.forConfig("p")` + `db.close()` on shutdown | `val (db, _) = Database.forConfig[IO]("p").allocated.unsafeRunSync()` + `db.close()` on shutdown | 3 — minimal change |
| `Await.result(db.shutdown, ...)` | Remove — use one of the options above | — |

### 5. `db.run`

| Pattern (Slick 3) | Replacement (Slick 4) |
|-------------------|-----------------------|
| `val f: Future[R] = db.run(a)` | `val io: IO[R] = db.run(a)` |
| `Await.result(db.run(a), d)` | `db.run(a).unsafeRunSync()` (in tests) or compose with `use` |

### 6. `db.stream`

| Pattern (Slick 3) | Replacement (Slick 4) |
|-------------------|-----------------------|
| `val p: DatabasePublisher[T] = db.stream(a)` | `val s: Stream[IO, T] = db.stream(a)` |
| `db.stream(a).foreach(f)` → `Future[Unit]` | `db.stream(a).evalMap(f).compile.drain` → `IO[Unit]` |
| `.mapResult(f)` on `DatabasePublisher` | `.map(f)` on `Stream[IO, T]` |
| `db.stream(a, bufferNext = false)` | `db.stream(a)` — the `bufferNext` overload is removed |

### 7. `ExecutionContext` parameters on `DBIOAction` methods

Remove `(implicit ec: ExecutionContext)` (or `(ec)`) from all calls to:

- `dbio.map(f)`
- `dbio.flatMap(f)`
- `dbio.filter(f)`
- `dbio.withFilter(f)`
- `dbio.cleanUp(f, keepFailure)`
- `dbio.zipWith(a)(f)`
- `DBIO.fold(actions, zero)(f)`

These methods no longer accept an EC parameter. If you pass one explicitly it will be a compile
error. If it was implicit it will simply no longer be required.

### 8. `DBIO.from` / `DBIO.liftF`

| Pattern (Slick 3) | Replacement (Slick 4) |
|-------------------|-----------------------|
| `DBIO.from(future: Future[R])` | `DBIO.from(io: IO[R])` — or any `F[R]` |
| `DBIO.from(Future(expr))` | `DBIO.from(IO(expr))` |
| `DBIO.from(Future.successful(v))` | `DBIO.successful(v)` (unchanged) or `DBIO.from(IO.pure(v))` |
| Bridge existing `Future`: `DBIO.from(fut)` | `DBIO.from(IO.fromFuture(IO(fut)))` |

`DBIO.liftF` is new in Slick 4 and is an alias for `DBIO.from`.

### 9. Transaction isolation

| Pattern (Slick 3) | Replacement (Slick 4) |
|-------------------|-----------------------|
| `a.transactionally` | `a.transactionally(ti)` |

### 10. `AsyncExecutor`

| Pattern (Slick 3) | Replacement (Slick 4) |
|-------------------|-----------------------|
| `AsyncExecutor("name", n, n, m, n)` | Remove entirely |
| `Database.forURL(url, executor=ae)` | `Database.forURL[IO](url)` — no executor parameter |
| `Database.forDataSource(ds, Some(n), executor=ae)` | `Database.forDataSource[IO](ds, Some(n))` |

### 11. `ioExecutionContext`

| Pattern (Slick 3) | Replacement (Slick 4) |
|-------------------|-----------------------|
| `db.ioExecutionContext` | Remove; use `db.io(thunk)` → `F[T]` |
| `Future(blocking(x))(db.ioExecutionContext)` | `db.io(x)` |

### 12. JMX bean name

| Pattern (Slick 3) | Replacement (Slick 4) |
|-------------------|-----------------------|
| `slick:type=AsyncExecutor,name=<poolName>` | Does not exist; use HikariCP beans |

### 13. Compile-error guide

If your project fails to compile after upgrading, use this table:

| Compile error | Cause | Fix |
|--------------|-------|-----|
| `overloaded method stream` or `too many arguments for method stream` | `stream(a, bufferNext)` overload removed | Use `db.stream(a)` — `bufferNext` is not needed in Slick 4 |
| `overloaded method flatMap with alternatives` or `could not find implicit value for parameter executor` | EC parameter removed | Remove `(implicit ec: ExecutionContext)` or `(ec)` from `flatMap`/`map` call |
| `type mismatch: found Future[R], required F[R]` | `db.run` return type changed | Remove `Await.result`, compose with `IO` |
| `value shutdown is not a member of Database` | method removed | Use `Resource.use`, `Resource.allocated` + `closeDb`, or `db.close()` — see [Database Construction](#2-database-construction) |
| `object AsyncExecutor is not a member of package slick.util` | class removed | Remove all `AsyncExecutor` usage |
| `value ioExecutionContext is not a member of Database` | field removed | Use `db.io(thunk)` |
| `could not find implicit value for parameter …: ClassTag[F]` on `DatabaseConfig.forConfig` | missing type parameter | Add `[P, IO]` type arguments |
| `class JdbcDatabaseDef takes type parameters` on a variable type annotation | explicit `JdbcDatabaseDef` type annotation from Slick 3 | Change `val db: JdbcDatabaseDef` to `val db: Database[IO]` and `Database.forURL(...)` to `Database.forURL[IO](...)` |
