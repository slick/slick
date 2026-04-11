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
- `DatabaseConfig.forConfig[P](path)` — still works with an added effect-type parameter; see [DatabaseConfig](#databaseconfig) below

### Changed (breaking)

| Area | Before (Slick 3) | After (Slick 4) |
|------|-----------------|-----------------|
| `db.run(a)` return type | `Future[R]` | `F[R]` (e.g. `IO[R]`) |
| `db.stream(a)` return type | `DatabasePublisher[T]` | `Stream[F, T]` |
| `Database` construction | `Database.forConfig("p")` returns `Database` | `Database.forConfig[IO]("p")` returns `Resource[IO, Database[IO]]` |
| `map`/`flatMap`/`filter`/`cleanUp`/`zipWith`/`DBIO.fold` | require `(implicit ec: ExecutionContext)` | no EC parameter |
| `DBIO.from(x)` | lifts a `Future[R]` | lifts any `F[R]` (CE3 effect) **or** `Future[R]` — both overloads exist |
| `DBIO.liftF` | did not exist | alias for `DBIO.from` |
| `.withTransactionIsolation(ti)` | sets isolation level, pins session | **removed** — use `.transactionally(ti)` |
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

Slick 4 requires **Scala 2.13** or **Scala 3** (primary). Scala 2.12 is no longer supported.

### Play / Akka / Pekko users (optional)

If your application uses `Future` at its framework boundary (Play controllers, Akka/Pekko actors),
add the optional compatibility module. It provides a `Future`-based `Database` wrapper with no
type parameter — identical to the Slick 3 API — so you can migrate with minimal changes:

```scala
libraryDependencies += "com.typesafe.slick" %% "slick-v3-compat" % "$project.version$"
```

See [Slick v3 Compatibility Layer](#slick-v3-compatibility-layer) for usage.

### Reactive Streams (included in `slick-v3-compat`)

The `reactive-streams` artifact is no longer a core dependency. `slick-v3-compat` includes the
`DatabasePublisher[T]` type alias and `db.streamAsPublisher` extension. If you are on a pure CE3
stack and only need Reactive Streams interop (without the `Future` wrapper), `slick-v3-compat`
still covers that use case via `import slick.compat.*`.

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

### Concurrency semantics: `Future` is eager, `IO` is lazy

@@@ warning
This is the most important behavioral difference when migrating to `Database[IO]`. Migrating
`Future` → `IO` without understanding this will silently serialize work that was previously
concurrent.

This section applies only to `Database[IO]` (the CE3-native API). If you are using
`slick.compat.Database`, its `run` method calls `unsafeToFuture()` internally and preserves
Slick 3's eager-start semantics — the concurrency behavior of your existing code is unchanged.
@@@

In Slick 3, `db.run(action)` **immediately** submitted work to the internal thread pool and
returned a pending `Future`. Assigning two `Future` values to `val`s started both operations
concurrently — this was "accidental" parallelism built into `Future`'s eager evaluation:

```scala
// Slick 3 — both operations start immediately at the `val` declaration
val dbR: Future[A]   = db.run(queryAction)
val httpR: Future[B] = httpClient.get(url)

// By the time we reach the for-comprehension, both are already running
for {
  row      <- dbR
  response <- httpR
} yield combine(row, response)
```

With `Database[IO]`, `db.run(action)` returns an `IO` value — a **description** of work to be
done. No database call is made until the `IO` is evaluated. A `for`-comprehension over `IO`
values is **sequential by construction** — each step waits for the previous one to complete:

```scala
// Slick 4 Database[IO] — SEQUENTIAL: httpClient.get does not start until db.run finishes
for {
  row      <- db.run(queryAction)
  response <- httpClient.get(url)
} yield combine(row, response)
```

To run independent `IO` operations concurrently, use the CE3 parallel combinators:

```scala
import cats.syntax.parallel.*

// Slick 4 Database[IO] — CONCURRENT: both start at the same time
(db.run(queryAction), httpClient.get(url)).parTupled.map {
  case (row, response) => combine(row, response)
}

// or equivalently:
(db.run(queryAction), httpClient.get(url)).parMapN(combine)
```

@@@ note { title="For AI agents performing automated migration" }
This section applies only when migrating to `Database[IO]`. If the target code uses
`slick.compat.Database` (which returns `Future`), the eager-start semantics are preserved and
no concurrency changes are needed.

When migrating Slick 3 code to `Database[IO]` that assigns multiple `db.run(...)` or
`Future`-producing calls to `val`s and then combines them in a single `for`-comprehension or
`Future.sequence`, the intent is almost always concurrent execution. Replace with `parTupled`,
`parMapN`, or `parSequence`:

| Pattern (Slick 3) | Replacement (Slick 4 `Database[IO]`) |
|-------------------|-----------------------|
| `val a = db.run(x); val b = db.run(y); for { ra <- a; rb <- b } yield f(ra, rb)` | `(db.run(x), db.run(y)).parMapN(f)` |
| `val a = db.run(x); val b = otherFuture; for { ra <- a; rb <- b } yield f(ra, rb)` | `(db.run(x), otherIO).parMapN(f)` |
| `Future.sequence(list.map(db.run(_)))` | `list.parTraverse(db.run(_))` |
| `Future.traverse(list)(db.run(_))` | `list.parTraverse(db.run(_))` |

If the original code uses a plain `for`-comprehension where each step depends on the result of the
previous one, **no change is needed** — both `Future` and `IO` are already sequential in that case.
@@@

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
longer pass it to DBIO combinators.

**Option A — `slick-v3-compat` (recommended for Play users staying on `Future`)**

Add `slick-v3-compat` to your dependencies. Swap the injection type, add two imports, and
everything else — `db.run`, `db.stream`, `DBIO.from(future)`, `db.close()` — is identical to
Slick 3:

```scala
import slick.jdbc.PostgresProfile.api.*
import slick.compat.*
import cats.effect.unsafe.implicits.runtime   // or inject IORuntime via DI

class UserController @Inject()(db: slick.compat.Database, cc: ControllerComponents)
    extends AbstractController(cc) {

  def list = Action.async {
    db.run(users.result)                        // Future[Seq[User]] — identical to Slick 3
      .map(rows => Ok(Json.toJson(rows)))(cc.executionContext)
  }

  def create = Action.async(parse.json) { req =>
    val action = for {
      id <- DBIO.from(generateId())             // generateId() returns Future[String], just works
      _  <- users += User(id, req.body.as[User])
    } yield id
    db.run(action).map(id => Created(id))
  }
}
```

Wire `slick.compat.Database` in your DI module:

```scala
// In a Play Module (Guice)
import cats.effect.unsafe.implicits.runtime
bind[slick.compat.Database].toInstance(slick.compat.Database.forConfig("mydb"))
```

**Option B — full CE3 migration**

Bridge `IO[R]` to `Future[R]` at the outermost point:

```scala
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
from DBIO combinators. It is still available for `pipeTo`, `ask`, and other `Future` operations.

**Option A — `slick-v3-compat` (recommended for actors staying on `Future`)**

```scala
import slick.compat.*
import cats.effect.unsafe.implicits.runtime   // or use a custom IORuntime

class OrderActor(db: slick.compat.Database) extends Actor {
  import context.dispatcher   // still needed for pipeTo / ask, not for DBIO

  def receive = {
    case PlaceOrder(order) =>
      val action = for {
        _ <- DBIO.from(validateOrder(order))  // validateOrder returns Future — just works
        id <- orders += order
      } yield id
      db.run(action).pipeTo(sender())         // Future — identical to Slick 3
  }
}
```

**Option B — full CE3 migration**

```scala
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

`DBIO.from` now accepts both a CE3 effect `F[R]` **and** a plain `Future[R]`. `DBIO.liftF` is
an alias for `DBIO.from`.

**`Future` overload — works unchanged from Slick 3:**

```scala
// Slick 3 — still compiles unchanged in Slick 4
val action = DBIO.from(callExternalService())   // callExternalService() returns Future[String]
```

No wrapping is required when you already have a `Future`. Scala resolves the `Future` overload
and the CE3 overload independently — there is no ambiguity.

**CE3 overload — lift any `F[R]`:**

```scala
// Slick 4 — preferred for new code
val action = DBIO.from(IO(computeSomething()))
// or equivalently:
val action = DBIO.liftF(IO(computeSomething()))
```

If you have a `Future` and want to bridge it into the CE3 world explicitly:

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

### `.withTransactionIsolation` is removed

Use the new `transactionally(ti: TransactionIsolation)` overload instead:

```scala
// Slick 3
action.transactionally.withTransactionIsolation(TransactionIsolation.Serializable)

// Slick 4
action.transactionally(TransactionIsolation.Serializable)
```

---

## 8. Savepoints (new in Slick 4)

Slick 4 adds first-class savepoint support via `.withSavepoint` on `DBIOAction` and the
lower-level `createSavepoint` / `releaseSavepoint` / `rollbackToSavepoint` primitives. These are
new features — there is nothing to migrate from Slick 3. See the
@ref:[Savepoints](dbio.md#savepoints) section in the DBIO chapter for full documentation.

---

## 9. `db.io` — running blocking work in `F`

If you need to run a blocking call on the CE3 blocking thread pool — outside of a DBIO action
— use `db.io`:

```scala
// Slick 4
val result: IO[String] = db.io(System.getProperty("user.home"))
```

`db.io(thunk)` is equivalent to `Async[F].blocking(thunk)`. It is the replacement for the
removed `db.ioExecutionContext`. Use `SimpleDBIO` for work that needs a `Connection`:

```scala
// For connection-aware blocking work, use SimpleDBIO (unchanged from Slick 3)
val action: DBIO[Boolean] = SimpleDBIO(_.connection.getAutoCommit)
db.run(action)  // IO[Boolean]
```

---

## 10. Reactive Streams Compatibility

If you need a `Publisher[T]` for interop with Akka Streams, Pekko Streams, or another Reactive
Streams consumer, there are two paths:

**Path 1 — `slick.compat.Database` (Future-based wrapper, from `slick-v3-compat`)**

`db.stream(action)` on `slick.compat.Database` returns a `Publisher[T]` directly — identical to
Slick 3. If you are migrating a Play / Akka / Pekko application, this is the easiest path. See
[Slick v3 Compatibility Layer](#slick-v3-compatibility-layer) for setup and full API.

**Path 2 — `db.streamAsPublisher` extension (CE3-native)**

If you have a `Database[IO]` and need a `Publisher[T]`, use the `streamAsPublisher` extension
from `import slick.compat.*`. It requires an implicit `Dispatcher[IO]` from your CE3 context:

```scala
import slick.compat.*

Dispatcher.parallel[IO].use { implicit dispatcher =>
  val publisher: Publisher[User] = db.streamAsPublisher(users.result)
  // hand to Akka/Pekko Streams: Source.fromPublisher(publisher)
  IO.unit
}
```

The `DatabasePublisher[T]` type alias (equal to `org.reactivestreams.Publisher[T]`) is provided
by `slick.compat.*` for drop-in source compatibility with code that used the Slick 3 type.

The core `slick` artifact no longer depends on `reactive-streams`.

---

## 11. Slick v3 Compatibility Layer

The optional `slick-v3-compat` module provides a `Future`-based `Database` type with no type
parameter — identical to the Slick 3 `Database` API. It is designed for Play / Akka / Pekko
applications that use `Future` at their framework boundary and want to migrate with minimal
diff.

### What `slick-v3-compat` provides

| Feature | Import / type |
|---------|--------------|
| `slick.compat.Database` — `Future`-based wrapper | `import slick.compat.*` |
| `db.run(action): Future[R]` | on `slick.compat.Database` |
| `db.stream(action): Publisher[T]` | on `slick.compat.Database` |
| `db.close(): Unit` | on `slick.compat.Database` |
| `db.underlying: Database[IO]` | escape hatch to access the full CE3 API |
| `DatabasePublisher[T]` type alias | `import slick.compat.*` |
| `db.streamAsPublisher(action)` extension | `import slick.compat.*` + implicit `Dispatcher[F]` |
| `DBIO.from(future: Future[R])` overload | **no import** — in `slick` core |

### Construction

All factory methods mirror Slick 3 exactly. They require an implicit
`cats.effect.unsafe.IORuntime` (you can import the global default or inject a custom one):

```scala
import cats.effect.unsafe.implicits.runtime   // global IORuntime — fine for most apps

// Identical to Slick 3 — no Resource, no type parameter
val db: slick.compat.Database = slick.compat.Database.forConfig("mydb")
val db: slick.compat.Database = slick.compat.Database.forDataSource(ds, Some(10))
val db: slick.compat.Database = slick.compat.Database.forURL("jdbc:h2:mem:test", driver="org.h2.Driver")
val db: slick.compat.Database = slick.compat.Database.forDriver(driver, url)
```

The connection pool and an internal `Dispatcher` are allocated eagerly on construction and
released when `db.close()` is called. `close()` is safe to call from a Play `Module` `onStop`
hook, an Akka extension shutdown callback, or a JVM shutdown hook — exactly as in Slick 3.

### `run` and `stream`

```scala
import slick.compat.*

// run — returns Future[R], identical to Slick 3
val result: Future[Seq[User]] = db.run(users.result)

// stream — returns Publisher[T], identical to Slick 3
val publisher: Publisher[User] = db.stream(users.result)
// e.g. hand to Akka Streams:
//   Source.fromPublisher(db.stream(users.result))
```

### `DBIO.from(future)` — no import needed

`DBIO.from` accepts a `Future[R]` directly in Slick 4 core (no `slick-v3-compat` needed for
this). This means that wherever you previously wrote:

```scala
// Slick 3
DBIO.from(callExternalService())   // callExternalService() returns Future[String]
```

...it **still compiles unchanged in Slick 4**. No `IO.fromFuture(IO(...))` wrapping is required
when the `Future` is already at hand. The CE3 overload `DBIO.from(io: F[R])` and the `Future`
overload coexist — Scala resolves them correctly.

```scala
// Still works unchanged in Slick 4 — no modification needed
val action: DBIO[String] = DBIO.from(callExternalService())
```

### Lifecycle — Play example

```scala
// app/modules/SlickModule.scala
import com.google.inject.{AbstractModule, Provides, Singleton}
import cats.effect.unsafe.implicits.runtime

class SlickModule extends AbstractModule {
  @Provides @Singleton
  def database(): slick.compat.Database =
    slick.compat.Database.forConfig("mydb")
}
```

Play calls `onStop` during shutdown; register a `Lifecycle` hook to call `db.close()`:

```scala
@Singleton
class SlickModule @Inject()(lifecycle: ApplicationLifecycle) extends AbstractModule {
  @Provides @Singleton
  def database(): slick.compat.Database = {
    val db = slick.compat.Database.forConfig("mydb")
    lifecycle.addStopHook(() => Future.successful(db.close()))
    db
  }
}
```

### CE3-managed `Database[IO]` with a `Future`-facing handle (`Database.wrap`)

If you already manage a `Database[IO]` via CE3 `Resource` (e.g. inside an `IOApp`) and only
need a `Future`-facing handle for one framework component, use `Database.wrap`:

```scala
Database.forConfig[IO]("mydb").use { underlying =>
  Dispatcher.parallel[IO].use { implicit dispatcher =>
    val db: slick.compat.Database = slick.compat.Database.wrap(underlying, dispatcher)
    // pass db to framework component; db.close() is a no-op — caller owns lifecycle
    runApp(db)
  }
}
```

### `streamAsPublisher` extension for CE3 users

For CE3-native code that has a `Database[F]` and needs a `Publisher[T]`:

```scala
import slick.compat.*

// Requires an implicit Dispatcher[F] in scope
Dispatcher.parallel[IO].use { implicit dispatcher =>
  val publisher: Publisher[User] = db.streamAsPublisher(users.result)
  Source.fromPublisher(publisher)  // hand to Akka/Pekko Streams
  IO.unit
}
```

---

## 12. Completely Removed APIs

The following APIs are removed and have **no direct replacement** in Slick 4 (the need they
addressed is either gone or covered by a different mechanism):

### `AsyncExecutor`

`AsyncExecutor`, `ManagedArrayBlockingQueue`, and `InternalArrayQueue` are deleted. CE3 manages
all threading internally. The `minThreads == maxThreads == maxConnections` constraint that was
required to prevent deadlocks in Slick 3 **does not exist in Slick 4**.

No code changes needed other than removing any `AsyncExecutor` construction. Do not pass it to
factory methods — none of the Slick 4 factory methods accept it.

Slick 4 replaces the old executor queue/thread model with explicit database execution limits:

- `queueSize` (default `1000`): max callers waiting to start.
- `maxInflightActions` (default `2 * maxConnections`): max concurrent DBIO chains.
- `maxConnections`: max concurrent JDBC work.
- `inflightAdmissionTimeout` (optional): bound waiting time for in-flight admission.
- `connectionAcquireTimeout` (optional): bound waiting time for connection-slot acquisition.

If you configured AsyncExecutor queue sizing in Slick 3, map that operational intent to
`queueSize` and `maxInflightActions` in Slick 4.

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
`slick-v3-compat` module (see [Reactive Streams Compatibility](#reactive-streams-compatibility)
and [Slick v3 Compatibility Layer](#slick-v3-compatibility-layer)).

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

## 13. ZIO Interop

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
- **Optional**: `"com.typesafe.slick" %% "slick-v3-compat" % "$project.version$"` — Future-based
  `Database` wrapper; see [Slick v3 Compatibility Layer](#slick-v3-compatibility-layer)
- **Remove** (or replace with `slick-v3-compat`): any direct dependency on
  `org.reactivestreams:reactive-streams`
- **Remove** Scala 2.12 cross-build entries; minimum is 2.13

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
| `val f: Future[R] = db.run(a)` | `val io: IO[R] = db.run(a)` — compose with CE3 |
| `val f: Future[R] = db.run(a)` | `val f: Future[R] = db.run(a)` — **unchanged** with `slick.compat.Database` |
| `Await.result(db.run(a), d)` | `db.run(a).unsafeRunSync()` (in tests) or compose with `use` |

For Play / Akka / Pekko applications staying on `Future`, add `slick-v3-compat` and use
`slick.compat.Database` — `db.run` returns `Future[R]` identically to Slick 3. See
[Slick v3 Compatibility Layer](#slick-v3-compatibility-layer).

### 5a. Concurrency — `Future` eagerness → `IO` laziness

This applies only to `Database[IO]`. `slick.compat.Database.run` returns `Future` and preserves
Slick 3's eager-start semantics — no concurrency changes are needed with the compat layer.

`IO` is lazy: independent `IO` values composed with `for`/`flatMap` run sequentially. Code that
relied on `Future`'s eager start for concurrency must be rewritten with CE3 parallel combinators.

| Pattern (Slick 3) | Replacement (Slick 4 `Database[IO]`) |
|-------------------|-----------------------|
| `val a = db.run(x); val b = db.run(y); for { ra <- a; rb <- b } yield f(ra, rb)` | `(db.run(x), db.run(y)).parMapN(f)` |
| `val a = db.run(x); val b = otherFuture; for { ra <- a; rb <- b } yield f(ra, rb)` | `(db.run(x), otherIO).parMapN(f)` |
| `Future.sequence(list.map(db.run(_)))` | `list.parTraverse(db.run(_))` |
| `Future.traverse(list)(db.run(_))` | `list.parTraverse(db.run(_))` |

If each step in the `for`-comprehension depends on the previous step's result, **no change is
needed** — both `Future` and `IO` are sequential in that case. See
[Concurrency semantics](#concurrency-semantics-future-is-eager-io-is-lazy) for full explanation.

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

`DBIO.from(future: Future[R])` compiles unchanged — the `Future` overload is present in `slick`
core alongside the CE3 overload. No migration is required for existing `DBIO.from` call sites.

| Pattern (Slick 3) | Status in Slick 4 | Notes |
|-------------------|--------------------|-------|
| `DBIO.from(future: Future[R])` | **unchanged** | `Future` overload in `slick` core |
| `DBIO.from(Future(expr))` | compiles unchanged; **preferably** replace with `DBIO.from(IO(expr))` | wrapping a thunk in `Future` is unnecessary overhead |
| `DBIO.from(Future.successful(v))` | compiles unchanged; prefer `DBIO.successful(v)` | `DBIO.successful` is the zero-overhead path |
| Bridge `Future` into CE3 explicitly | `DBIO.from(IO.fromFuture(IO(fut)))` | only needed if you want the result as `IO` before passing to `DBIO.from` |

`DBIO.liftF` is new in Slick 4 and is an alias for `DBIO.from`.

### 9. Transaction isolation

| Pattern (Slick 3) | Replacement (Slick 4) |
|-------------------|-----------------------|
| `a.transactionally.withTransactionIsolation(ti)` | `a.transactionally(ti)` |
| `a.withTransactionIsolation(ti)` | `a.transactionally(ti)` |

`.withTransactionIsolation` does not exist in Slick 4 and will cause a compile error.

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
| `value withTransactionIsolation is not a member of DBIOAction` | method removed | Use `.transactionally(ti)` |
| `type mismatch: found Future[R], required F[R]` | `db.run` return type changed | Remove `Await.result`, compose with `IO` |
| `value shutdown is not a member of Database` | method removed | Use `Resource.use`, `Resource.allocated` + `closeDb`, or `db.close()` — see [Database Construction](#2-database-construction) |
| `object AsyncExecutor is not a member of package slick.util` | class removed | Remove all `AsyncExecutor` usage |
| `value ioExecutionContext is not a member of Database` | field removed | Use `db.io(thunk)` |
| `could not find implicit value for parameter …: ClassTag[F]` on `DatabaseConfig.forConfig` | missing type parameter | Add `[P, IO]` type arguments |
| `class JdbcDatabaseDef takes type parameters` on a variable type annotation | explicit `JdbcDatabaseDef` type annotation from Slick 3 | Change `val db: JdbcDatabaseDef` to `val db: Database[IO]` and `Database.forURL(...)` to `Database.forURL[IO](...)` |
