Migrating to Slick 4
====================

@@@ note
This guide is written for both human readers and AI agents performing automated migrations. The
[Mechanical Migration Checklist](#mechanical-migration-checklist) at the end contains a concise,
pattern-by-pattern reference suitable for automated tooling.

For upgrade notes between Slick 3.x minor versions, see @ref:[Slick 3.x Upgrade Notes](upgrade-slick3.md).
@@@

Slick 4 replaces the `Database.forXxx` factory methods and `scala.concurrent.Future`-returning
execution layer with a new, three-facade API. The query DSL, SQL compiler pipeline,
`DBIOAction` type and phantom effect system (`Effect.Read`/`Write`/`Schema`/`Transactional`), all
database profiles, and all SQL generation code are **completely unchanged**. If your application
only composes queries and runs them, the migration is largely mechanical.

Slick 4 ships three ready-made facades:

| Facade | Module | Effect type | Stream type |
|--------|--------|-------------|-------------|
| `slick.future.Database` | `slick-future` | `scala.concurrent.Future` | Reactive Streams `DatabasePublisher[T]` |
| `slick.cats.Database` | `slick` (core) | `cats.effect.IO` | `fs2.Stream[IO, T]` |
| `slick.zio.Database` | `slick-zio` | `zio.Task` | `zio.stream.ZStream[Any, Throwable, T]` |

**If you are migrating from Slick 3 and are not already using Cats Effect or ZIO, use the
`slick-future` module.** It keeps `Future` and Reactive Streams exactly where they were — the
only thing that changes is how you construct the database.

---

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

### Changed (breaking)

| Area | Before (Slick 3) | After (Slick 4, `slick-future`) |
|------|-----------------|--------------------------------|
| `db.run(a)` return type | `Future[R]` | `Future[R]` — **unchanged** |
| `db.stream(a)` return type | `DatabasePublisher[T]` (subclass with `.foreach`/`.mapResult`) | `DatabasePublisher[T]` — unchanged; `bufferNext` overload removed |
| `Database` construction | `Database.forConfig("p")` | `DatabaseConfig.forConfig[P]("p")` → `Database.open(dc)` or `Database.use(dc)(f)` |
| `db.shutdown` | `Future[Unit]` | **removed** — use `Database.use` or `db.close()` |
| `AsyncExecutor` | required for thread pool config | **removed** entirely |
| `map`/`flatMap` on `DBIOAction` | require `(implicit ec: ExecutionContext)` | no EC parameter |
| `DBIO.from(x)` | lifts a `Future[R]` | lifts a `Future[R]` — **unchanged** in `slick-future` |

---

## 1. Dependencies

### Add `slick-future`

The Future facade lives in a separate module. Add it to your build:

@@@ vars
```scala
libraryDependencies += "com.typesafe.slick" %% "slick-future" % "$project.version$"
```
@@@

Keep your existing `slick` and `slick-hikaricp` dependencies. Remove any direct dependency on
`org.reactivestreams:reactive-streams` — it is now pulled in transitively by `slick-future`.

### Scala version

Slick 4 supports **Scala 2.12**, **Scala 2.13**, and **Scala 3** (primary).

---

## 2. Database Construction

In Slick 3 you called `Database.forConfig("mydb")` (or another `forXxx` method) directly.
In Slick 4 these methods are **removed**. The new entry point is `slick.jdbc.DatabaseConfig`,
which bundles the database profile with the connection configuration and returns a
`JdbcDatabaseConfig` value. You then open a `Database` from it.

```scala
import slick.jdbc.DatabaseConfig   // the one to use for all JDBC databases
import slick.jdbc.H2Profile
import slick.future.Database
```

`slick.jdbc.DatabaseConfig` provides factory methods mirroring those that existed on `Database`:

| Method | Description |
|--------|-------------|
| `DatabaseConfig.forURL(H2Profile, url, ...)` | JDBC URL |
| `DatabaseConfig.forDataSource(H2Profile, ds, maxConn)` | Existing `DataSource` |
| `DatabaseConfig.forName(H2Profile, jndi, maxConn)` | JNDI name |
| `DatabaseConfig.forProfileConfig(H2Profile, "mydb")` | Typesafe Config path with a concrete profile |
| `DatabaseConfig.forConfig[H2Profile.type]("mydb")` | Typesafe Config path, profile class loaded from config |

All of these return a `JdbcDatabaseConfig` that you pass to `Database.open` or `Database.use`.

There are two ways to open a `slick.future.Database` from a `JdbcDatabaseConfig`:

- **`Database.use(dc)(f)`** — opens a **new** database instance (and its connection pool),
  runs `f` with it, then closes it when the `Future` completes. Use this to wrap your **whole
  program** — not individual queries. Calling it per-request or per-query opens and tears down
  the connection pool on every call, which is both slow and wrong.
- **`Database.open(dc)`** — returns `Future[Database]`; you are responsible for calling
  `db.close()` once when your application shuts down.

### Option 1 — `Database.use` (for scripts and top-level programs)

`Database.use` is the right choice when your entire program fits inside a single `Future` —
for example a batch job, a migration script, or a `main` method:

```scala
// Slick 3
val db = Database.forConfig("mydb")
try {
  Await.result(program(db), Duration.Inf)
} finally {
  db.close()
}

// Slick 4 — equivalent one-liner for a self-contained program
import slick.future.Database
import slick.jdbc.{DatabaseConfig, H2Profile}

val dc = DatabaseConfig.forProfileConfig(H2Profile, "mydb")
Await.result(
  Database.use(dc) { db => program(db) },
  Duration.Inf
)
```

The connection pool is opened once, your program runs, and the pool is closed when the
`Future` completes — whether successfully or with a failure.

@@@ warning
Do **not** call `Database.use` inside a request handler or around individual queries. Each
call to `use` opens and closes the connection pool. For a long-running server use
`Database.open` instead and keep the resulting `Database` for the lifetime of the application.
@@@

### Option 2 — `Database.open` + manual `close()` (for long-running applications)

For servers, Play applications, Akka/Pekko systems, or any code where `db` is held for the
lifetime of the application, use `Database.open` once at startup and `db.close()` once at
shutdown:

```scala
val dc = DatabaseConfig.forProfileConfig(H2Profile, "mydb")
val dbFuture: Future[Database] = Database.open(dc)

// Pass db to your services, route handlers, actors, etc.
// On application shutdown:
dbFuture.foreach(_.close())
```

This is the direct replacement for the old `val db = Database.forConfig("mydb")` / `db.close()`
pattern and the right approach for most production applications.

---

## 3. `db.run` and `db.stream`

Once you have a `slick.future.Database`, `run` and `stream` return the same types as before:

```scala
// Slick 3 and Slick 4 with slick-future — identical
val result: Future[Seq[User]]           = db.run(users.result)
val publisher: DatabasePublisher[User]  = db.stream(users.result)
```

All existing code that wires the `Publisher` to a Reactive Streams subscriber (Akka Streams,
Pekko Streams, etc.) continues to work without changes.

---

## 4. `ExecutionContext` removal from `DBIOAction` combinators

`map`, `flatMap`, `filter`, `withFilter`, `cleanUp`, `zipWith`, and `DBIO.fold` **no longer
accept an implicit `ExecutionContext`**. Remove all EC imports and implicits that were only
present for DBIO composition.

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

Keep any `ExecutionContext` imports that are used for non-Slick `Future` code in the same file.

### `DBIO.fold`

`DBIO.fold` no longer takes an `ExecutionContext`:

```scala
// Slick 3
DBIO.fold(actions, zero)(f)(ec)

// Slick 4
DBIO.fold(actions, zero)(f)
```

---

## 5. `DBIO.from`

`DBIO.from` continues to lift a `Future[R]` into a `DBIOAction` when using `slick-future`.
No change is needed here.

```scala
// Unchanged between Slick 3 and Slick 4 (slick-future)
val action = DBIO.from(Future(computeSomething()))
```

---

## 6. Transaction API — unchanged

`.transactionally` works identically to before:

```scala
val transfer: DBIO[Unit] = (for {
  _ <- accounts.filter(_.id === from).map(_.balance).update(fromBal - amount)
  _ <- accounts.filter(_.id === to).map(_.balance).update(toBal + amount)
} yield ()).transactionally

val fut: Future[Unit] = db.run(transfer)
```

---

## 7. Completely Removed APIs

### `Database.forXxx` factory methods

`Database.forConfig`, `Database.forURL`, `Database.forDataSource`, and `Database.forName` are
**removed**. Use `slick.jdbc.DatabaseConfig.forURL` / `forDataSource` / `forName` /
`forProfileConfig`, then call `Database.open` or `Database.use` — see
[Database Construction](#2-database-construction).

### `db.shutdown`

`shutdown: Future[Unit]` is removed. Replace with `db.close()` (synchronous, same effect), or
`Future(db.close())` if you need a `Future[Unit]` to compose with.

### `AsyncExecutor`

`AsyncExecutor`, `ManagedArrayBlockingQueue`, and `InternalArrayQueue` are deleted. Remove any
`AsyncExecutor` construction from your code; none of the Slick 4 factory methods accept it.

The `minThreads == maxThreads == maxConnections` constraint that was required to prevent
deadlocks in Slick 3 **does not exist in Slick 4**. You are free to size your HikariCP pool
based on what your database server can handle, independent of thread counts.

Slick 4 replaces the old executor queue model with explicit limits:

- `queueSize` (default `1000`): max callers waiting to start.
- `maxInflightActions` (default `2 * maxConnections`): max concurrent DBIO chains.
- `maxConnections`: max concurrent JDBC connections.

If you configured AsyncExecutor queue sizing in Slick 3, map that intent to `queueSize` and
`maxInflightActions` in Slick 4.

### JMX `AsyncExecutorMXBean`

The `slick:type=AsyncExecutor,name=…` JMX bean is removed. HikariCP's own JMX beans
(`com.zaxxer.hikari:type=Pool` and `com.zaxxer.hikari:type=PoolConfig`) still work unchanged.
For programmatic inspection of Slick's admission and connection-slot counters, use
`db.controlStatus: Future[ControlStatus]`, which exposes `availableConnectionSlots`,
`pendingConnectionSlots`, `availableAdmissionQueueSlots`, and `availableInflightSlots`.

### `db.stream(action, bufferNext)` overload

The two-argument `stream(action, bufferNext: Boolean)` overload is removed. In Slick 3,
`bufferNext = false` kept the result set open while an async consumer processed each row,
which was required to keep LOB references (e.g. `java.sql.Blob`) valid on databases such as
DB2.

In Slick 4 the pull model is inherently demand-driven. Use `db.stream(action)` — no flag is
needed or accepted.

```scala
// Slick 3
db.stream(blobQuery.result, bufferNext = false)

// Slick 4
db.stream(blobQuery.result)   // demand-driven by default
```

---

## 8. Cats Effect and ZIO

If you want to migrate to Cats Effect 3 or ZIO in the future, Slick 4 supports both natively.
See @ref:[Database](database.md) for the `slick.cats.Database` and `slick.zio.Database` APIs.

---

## Mechanical Migration Checklist

This section is a concise, pattern-by-pattern reference for automated migration tooling or for
developers doing a systematic find-and-replace pass.

All patterns below assume you are migrating to `slick-future` (Future + Reactive Streams).

### 1. Dependencies — `build.sbt` / `pom.xml`

- **Add**: `"com.typesafe.slick" %% "slick-future" % "<version>"`
- **Remove** (or leave — it is now a transitive dependency): direct `org.reactivestreams:reactive-streams`
- **Keep** all existing `slick` and `slick-hikaricp` entries unchanged

### 2. Imports to add

```scala
import slick.future.Database
import slick.jdbc.DatabaseConfig   // slick.jdbc.DatabaseConfig — the one with all factory methods
```

### 3. Imports to remove

Remove any import that was only present to provide an `ExecutionContext` for DBIO composition.
Keep EC imports that are used for non-Slick `Future` code in the same file.

```
import scala.concurrent.ExecutionContext
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.ExecutionContext.parasitic
```

### 4. Database construction

`Database.forXxx` is removed. Replace with `slick.jdbc.DatabaseConfig.forXxx` + `Database.use` or
`Database.open`:

| Pattern (Slick 3) | Replacement (Slick 4, `slick-future`) |
|-------------------|-----------------------------------------|
| `Database.forConfig("p")` | `DatabaseConfig.forProfileConfig(MyProfile, "p")` or `DatabaseConfig.forConfig[MyProfile.type]("p")` |
| `Database.forURL(url, driver=d)` | `DatabaseConfig.forURL(MyProfile, url, driver=d)` |
| `Database.forDataSource(ds, Some(n))` | `DatabaseConfig.forDataSource(MyProfile, ds, Some(n))` |
| `Database.forName(jndi, Some(n))` | `DatabaseConfig.forName(MyProfile, jndi, Some(n))` |

### 5. Database lifecycle

`Database.use` opens a **new** connection pool each invocation — use it only to wrap a whole
program (batch job, script, `main`). For long-running servers use `Database.open` once at
startup.

| Pattern (Slick 3) | Replacement (Slick 4, `slick-future`) | Notes |
|-------------------|-----------------------------------------|-------|
| `val db = Database.forConfig("p")` + `db.close()` at shutdown | `val dbFut = Database.open(dc)` + `dbFut.foreach(_.close())` at shutdown | Normal server pattern |
| `val db = …` + `Await.result(program(db), …)` + `db.close()` | `Await.result(Database.use(dc)(program), …)` | Scripts / batch jobs only |
| `Await.result(db.shutdown, ...)` | `db.close()` | |

### 6. `db.run`

No change needed. `db.run(action)` still returns `Future[R]`.

| Pattern (Slick 3) | Replacement (Slick 4) |
|-------------------|-----------------------|
| `val f: Future[R] = db.run(a)` | unchanged |
| `Await.result(db.run(a), d)` | unchanged |

### 7. `db.stream`

`db.stream(action)` returns a `DatabasePublisher[T]` with `.foreach` and `.mapResult` working
as in Slick 3. The only breaking change is the removal of the `bufferNext` overload:

| Pattern (Slick 3) | Replacement (Slick 4) |
|-------------------|-----------------------|
| `db.stream(a, bufferNext = false)` | `db.stream(a)` — `bufferNext` overload removed |

### 8. `ExecutionContext` parameters on `DBIOAction` methods

Remove `(implicit ec: ExecutionContext)` (or `(ec)`) from all calls to:

- `dbio.map(f)`
- `dbio.flatMap(f)`
- `dbio.filter(f)`
- `dbio.withFilter(f)`
- `dbio.cleanUp(f, keepFailure)`
- `dbio.zipWith(a)(f)`
- `DBIO.fold(actions, zero)(f)`

If passed implicitly it will simply no longer be resolved. If passed explicitly it will be a
compile error.

### 9. `DBIO.from`

No change needed. `DBIO.from(future: Future[R])` is unchanged in `slick-future`.

### 10. `AsyncExecutor`

| Pattern (Slick 3) | Replacement (Slick 4) |
|-------------------|-----------------------|
| `AsyncExecutor("name", n, n, m, n)` | Remove entirely |
| `Database.forURL(url, executor=ae)` | `DatabaseConfig.forURL(profile, url)` — no executor parameter |
| `Database.forDataSource(ds, Some(n), executor=ae)` | `DatabaseConfig.forDataSource(profile, ds, Some(n))` |

### 11. JMX bean name

| Pattern (Slick 3) | Replacement (Slick 4) |
|-------------------|-----------------------|
| `slick:type=AsyncExecutor,name=<poolName>` | Use HikariCP JMX beans for pool metrics; use `db.controlStatus` for Slick admission/connection-slot counters |

### 12. Compile-error guide

If your project fails to compile after upgrading, use this table:

| Compile error | Cause | Fix |
|--------------|-------|-----|
| `overloaded method stream` or `too many arguments for method stream` | `stream(a, bufferNext)` overload removed | Use `db.stream(a)` |
| `could not find implicit value for parameter executor` | EC parameter removed from DBIO combinators | Remove `(implicit ec: ExecutionContext)` or `(ec)` from `flatMap`/`map` call |
| `value shutdown is not a member of Database` | `db.shutdown` removed | Use `db.close()`, or `Future(db.close())` if you need a `Future[Unit]` |
| `object AsyncExecutor is not a member of package slick.util` | class removed | Remove all `AsyncExecutor` usage |
| `object Database is not a member of package slick.jdbc` or `value forConfig is not a member of ...` | `Database.forXxx` removed | Use `DatabaseConfig.forXxx` — see [Database Construction](#2-database-construction) |
| `class JdbcDatabaseDef takes type parameters` on a variable type annotation | explicit `JdbcDatabaseDef` type annotation from Slick 3 | Change to `val db: slick.future.Database` and migrate factory call |
