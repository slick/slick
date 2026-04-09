Database I/O Actions
====================

Anything that you can execute on a database, whether it is a getting the result of a query
(`myQuery.result`), creating a table (`myTable.schema.create`), inserting data
(`myTable += item`) or something else, is an instance of
@scaladoc[DBIOAction](slick.dbio.DBIOAction), parameterized by the result type it will produce when you execute it.

*Database I/O Actions* can be combined with several different combinators (see the
@scaladoc[DBIOAction class](slick.dbio.DBIOAction) and @scaladoc[DBIOAction object](slick.dbio.DBIOAction$), which is also
available under the alias `DBIO`, for details), but they will always be executed strictly sequentially and (at least
conceptually) in a single database session.

In most cases you will want to use the type aliases @scaladoc[DBIO](slick.dbio.package#DBIO[+R]:DBIO[R])
and @scaladoc[StreamingDBIO](slick.dbio.package#StreamingDBIO[+R,+T]:StreamingDBIO[R,T]) for non-streaming and
streaming Database I/O Actions. They omit the optional *effect types* supported by @scaladoc[DBIOAction](slick.dbio.DBIOAction).

@@@ note

In the code examples below we assume the following imports:

@@snip [Connection.scala](../code/Connection.scala) { #imports }

If you're new to Slick, please start with the  @ref:[Getting Started](gettingstarted.md) page.

@@@

Executing Database Actions
------------------------------

`DBIOAction`s can be executed either with the goal of producing a fully materialized result or streaming
data back from the database.

### Materialized

You can use `run` to execute a `DBIOAction` on a Database and produce a materialized result. This can
be, for example, a scalar query result (`myTable.length.result`), a collection-valued query
result (`myTable.to[Set].result`), or any other action. Every `DBIOAction` supports this mode of
execution.

Execution of the action starts in the background when `run` is called. The calling thread is not blocked. The
materialized result is returned as an `F[R]` value (for example `IO[Seq[User]]`) that completes
asynchronously and can be composed with the rest of your effect-based program:

@@snip [Connection.scala](../code/Connection.scala) { #materialize }

### Streaming

Collection-valued queries also support streaming results. In this case, the actual collection type
is ignored and elements are streamed directly from the result set through an @extref[FS2](fs2:)
`Stream[F, T]`, which provides structural back-pressure without requiring any Reactive Streams
subscription protocol.

Execution of the `DBIOAction` does not start until the `Stream` is consumed. If the `Stream` is
consumed multiple times, each consumption triggers an *independent execution* of the action.

Stream elements are signaled as soon as they become available in the streaming part of the `DBIOAction`. The end of
the stream is signaled only after the *entire action* has completed. For example, when streaming inside a transaction
and all elements have been delivered successfully, the stream can still fail afterwards if the transaction cannot be
committed.

@@snip [Connection.scala](../code/Connection.scala) { #stream }

When streaming a JDBC result set, each `iterator.next()` call is wrapped in `F.blocking`, so the
OS thread is only occupied during the actual row fetch. LOB values (such as `Blob`) are safe to
access synchronously on the element because the result set pointer is not advanced until the
consumer requests the next element:

@@snip [Connection.scala](../code/Connection.scala) { #streamblob }

@@@ note
Some database systems may require session parameters to be set in a certain way to support streaming without
caching all data at once in memory on the client side. For example, @extref[PostgreSQL](postgresql:) requires both
`.withStatementParameters(rsType = ResultSetType.ForwardOnly, rsConcurrency = ResultSetConcurrency.ReadOnly, fetchSize = n)`
(with the desired page size `n`) and `.transactionally` for proper streaming.
@@@

Composing Database I/O Actions
------------------------------

`DBIOAction`s describe sequences of individual actions to execute in strictly sequential order on
one database session (at least conceptually), therefore the most commonly used combinators deal with
sequencing. Since a `DBIOAction` eventually results in a `Success` or `Failure`, its combinators
have to distinguish between successful and failed executions. Unless specifically noted, all
combinators only apply to successful actions. Any failure aborts the sequence of execution.

### Sequential Execution

The simplest combinator is @scaladoc[DBIO.seq](slick.dbio.DBIOAction$#seq[E%3C:Effect](DBIOAction[_,NoStream,E]*):DBIOAction[Unit,NoStream,E])
which takes a varargs list of actions to run in sequence, discarding their return value. If you
need the return value, you can use @scaladoc[andThen](slick.dbio.DBIOAction#andThen[R2,S2%3C:NoStream,E2%3C:Effect](DBIOAction[R2,S2,E2]):DBIOAction[R2,S2,EwithE2])
to combine two actions and keep the result of the second one. If you need both return values of two
actions, there is the @scaladoc[zip](slick.dbio.DBIOAction#zip[R2,E2%3C:Effect](DBIOAction[R2,NoStream,E2]):DBIOAction[(R,R2),NoStream,EwithE2])
combinator. For getting all result values from a sequence of actions (of compatible types), use
@scaladoc[DBIO.sequence](slick.dbio.DBIOAction$#sequence[R,M[+_]%3C:TraversableOnce[_],E%3C:Effect](M[DBIOAction[R,NoStream,E]])(CanBuildFrom[M[DBIOAction[R,NoStream,E]],R,M[R]]):DBIOAction[M[R],NoStream,E]).
All these combinators work with pre-existing `DBIOAction`s which are composed eagerly:

@@snip [DBIOCombinators.scala](../code/DBIOCombinators.scala) { #combinators1 }

If an action depends on a previous action in the sequence, you have to compute it on the fly with
@scaladoc[flatMap](slick.dbio.DBIOAction#flatMap[R2,S2%3C:NoStream,E2%3C:Effect]((R)=%3EDBIOAction[R2,S2,E2]):DBIOAction[R2,S2,EwithE2])
or @scaladoc[map](slick.dbio.DBIOAction#map[R2]((R)=%3ER2):DBIOAction[R2,NoStream,E]).
These two methods plus @scaladoc[filter](slick.dbio.DBIOAction#filter((R)=%3EBoolean):DBIOAction[R,NoStream,E])
enable the use of *for comprehensions* for action sequencing.

Similar to @scaladoc[DBIO.sequence](slick.dbio.DBIOAction$#sequence[R,M[+_]%3C:TraversableOnce[_],E%3C:Effect](M[DBIOAction[R,NoStream,E]])(CanBuildFrom[M[DBIOAction[R,NoStream,E]],R,M[R]]):DBIOAction[M[R],NoStream,E])
for upfront composition, there is @scaladoc[DBIO.fold](slick.dbio.DBIOAction$#fold[T,E%3C:Effect](Seq[DBIOAction[T,NoStream,E]],T)((T,T)=%3ET):DBIOAction[T,NoStream,E])
for working with sequences of actions and composing them based on the previous result.

### Error Handling

You can use @scaladoc[andFinally](slick.dbio.DBIOAction#andFinally[E2%3C:Effect](DBIOAction[_,NoStream,E2]):DBIOAction[R,S,EwithE2])
to perform a cleanup action, no matter whether the previous action succeeded or failed. This is similar to using
`try ... finally ...` in imperative Scala code. A more flexible version of
@scaladoc[andFinally](slick.dbio.DBIOAction#andFinally[E2%3C:Effect](DBIOAction[_,NoStream,E2]):DBIOAction[R,S,EwithE2])
is @scaladoc[cleanUp](slick.dbio.DBIOAction#cleanUp[E2%3C:Effect]((Option[Throwable])=%3EDBIOAction[_,NoStream,E2],Boolean):DBIOAction[R,S,EwithE2]).
It lets you transform the failure and decide how to fail the resulting action if both the original
one and the cleanup failed.

@@@ note
For even more flexible error handling use
@scaladoc[asTry](slick.dbio.DBIOAction#asTry:DBIOAction[Try[R],NoStream,E])
and @scaladoc[failed](slick.dbio.DBIOAction#failed:DBIOAction[Throwable,NoStream,E]). Unlike with
@scaladoc[andFinally](slick.dbio.DBIOAction#andFinally[E2%3C:Effect](DBIOAction[_,NoStream,E2]):DBIOAction[R,S,EwithE2])
and @scaladoc[cleanUp](slick.dbio.DBIOAction#cleanUp[E2%3C:Effect]((Option[Throwable])=%3EDBIOAction[_,NoStream,E2],Boolean):DBIOAction[R,S,EwithE2])
the resulting actions cannot be used for streaming.
@@@

@@@ note
**Cancellation behaviour**: `cleanUp` and `andFinally` run their cleanup actions on fiber
cancellation — `cleanUp` receives `Some(CancellationException)` so you can distinguish
cancellation from errors. After cleanup completes, the fiber remains canceled.

`asTry` and `failed` do *not* intercept cancellation. If the underlying action is canceled,
the fiber stays canceled and downstream `flatMap` continuations do not run.
@@@

### Primitives

You can lift any CE3 effect `F[R]` into an action with @scaladoc[DBIO.from](slick.dbio.DBIOAction$#from[F[_],R](F[R]):DBIOAction[R,NoStream,Effect]).
`DBIO.liftF` is an alias for `DBIO.from`. This allows an `IO` (or any other `F[_]: Async` value)
to be used in an action sequence:

```scala
val action: DBIO[String] = for {
  id <- DBIO.from(IO(java.util.UUID.randomUUID().toString))
  _  <- users += User(id, "Alice")
} yield id
```

A pre-existing value or failure can be converted with
@scaladoc[DBIO.successful](slick.dbio.DBIOAction$#successful[R](R):DBIOAction[R,NoStream,Effect])
and @scaladoc[DBIO.failed](slick.dbio.DBIOAction$#failed(Throwable):DBIOAction[Nothing,NoStream,Effect]), respectively.

### Debugging

The @scaladoc[named](slick.dbio.DBIOAction#named(String):DBIOAction[R,S,E]) combinator names an
action. This name can be seen in debug logs if you enable the `slick.basic.BasicBackend.action`  @ref:[logger](config.md#logging).

### Transactions and Pinned Sessions {#transactions}

When executing a `DBIOAction` which is composed of several smaller actions, Slick acquires sessions from the connection
pool and releases them again as needed so that a session is not kept in use unnecessarily while waiting for the result
of a non-database computation (e.g. the function passed to
@scaladoc[flatMap](slick.dbio.DBIOAction#flatMap[R2,S2%3C:NoStream,E2%3C:Effect]((R)=%3EDBIOAction[R2,S2,E2]):DBIOAction[R2,S2,EwithE2])
that determines the next action to run). You can use
@scaladoc[withPinnedSession](slick.dbio.DBIOAction#withPinnedSession:DBIOAction[R,S,E]) to force the use of a single
session, keeping the existing session open even when waiting for non-database computations.

All @scaladoc[DBIOAction combinators](slick.dbio.DBIOAction) which combine database actions without any non-database
computations in between (e.g.
@scaladoc[andThen](slick.dbio.DBIOAction#andThen[R2,S2%3C:NoStream,E2%3C:Effect](DBIOAction[R2,S2,E2]):DBIOAction[R2,S2,EwithE2])
or
@scaladoc[zip](slick.dbio.DBIOAction#zip[R2,E2%3C:Effect](DBIOAction[R2,NoStream,E2]):DBIOAction[(R,R2),NoStream,EwithE2])
applied to two database computations) can fuse these actions for more efficient execution, with the side-effect that
the fused action runs inside a single session, even without `withPinnedSession`.

There is a related combinator called
@scaladoc[transactionally](slick.jdbc.JdbcActionComponent$JdbcActionExtensionMethods#transactionally:DBIOAction[R,S,EwithTransactional])
to force the use of a transaction. This guarantees that the entire `DBIOAction` that is executed will
either succeed or fail atomically. Without it, all database actions run in auto-commit mode. The use of a transaction
always implies a pinned session.

An overload
@scaladoc[transactionally(ti)](slick.jdbc.JdbcActionComponent$JdbcActionExtensionMethods#transactionally(TransactionIsolation):DBIOAction[R,S,EwithTransactional])
accepts a `TransactionIsolation` level, replacing the old `.withTransactionIsolation(ti)` combinator:

```scala
action.transactionally(TransactionIsolation.Serializable)
```

@@snip [Connection.scala](../code/Connection.scala) { #transaction }

@@@ warning
Warning: Failure is not guaranteed to be atomic *at the level of an individual* `DBIOAction` that is wrapped with
`transactionally`, so you need to be careful where you apply error recovery combinators. An actual database
transaction is only created and committed or rolled back for the outermost `transactionally` action. Nested
`transactionally` actions simply execute inside the existing transaction without additional savepoints.
@@@

Use @ref:[savepoints](#savepoints) if you need partial rollback within a transaction.

@@@ note
**Cancellation guarantee**: in Slick 4 a transaction is rolled back not only on error but also on
fiber cancellation. This guarantee was not possible with `Future`-based execution.
@@@

### Rollbacks

In case you want to force a rollback, you can return `DBIO.failed` within a `DBIOAction`.

@@snip [Connection.scala](../code/Connection.scala) { #rollback }

### Savepoints

Savepoints allow partial rollback within a transaction. `.withSavepoint` wraps an action with a
JDBC savepoint: if the action **succeeds**, the savepoint is released and the result is returned
normally; if it **fails**, the action's writes are rolled back to the savepoint and the **original
exception is re-thrown**.

Because the error is re-thrown, a bare `_ <- action.withSavepoint` in a for-comprehension still
short-circuits on failure. To let the surrounding transaction continue after a savepoint failure,
handle the error explicitly. Two common approaches:

**`.asTry`** — converts the outcome to a `Try[R]`, which is always a `Success` or `Failure` value
and never short-circuits the for-comprehension. Note that `.asTry` converts errors to `Failure`
values but does not intercept fiber cancellation — if the fiber is canceled, the `asTry` result
is never produced.

```scala
// Surrounding transaction continues even if the duplicate insert fails.
// The failed insert's writes are rolled back; item-A and item-B writes are preserved.
(for {
  _ <- orders += Order(1, "item-A")
  _ <- (orders += Order(1, "duplicate")).withSavepoint.asTry  // Failure(...) returned, not thrown
  _ <- orders += Order(2, "item-B")
} yield ()).transactionally
```

**`.recover`** — provides a fallback value for specific failures, continuing the transaction with
that value instead:

```scala
(for {
  _ <- orders += Order(1, "item-A")
  _ <- (orders += Order(1, "duplicate")).withSavepoint
         .recover { case _: SQLException => 0 }  // substitute a value on failure
  _ <- orders += Order(2, "item-B")
} yield ()).transactionally
```

Without `.asTry` or `.recover`, a failure inside `.withSavepoint` rolls back the savepoint's
writes but still short-circuits the for-comprehension, and the surrounding `.transactionally`
then rolls back everything.

The high-level `.withSavepoint` extension is available from any `JdbcProfile`. Low-level
primitives for manual control are also available: `createSavepoint`, `releaseSavepoint(sp)`, and
`rollbackToSavepoint(sp)`. These must be called inside a `.transactionally` scope.

Note: savepoints require JDBC driver support. SQLite does not support them and will throw at
runtime.

JDBC Interoperability
---------------------

In order to drop down to the JDBC level for functionality that is not available in Slick, you can
use a `SimpleDBIO` action which is run on a database thread and gets access to the JDBC `Connection`:

@@snip [Connection.scala](../code/Connection.scala) { #simpleaction }

If you need to access state of the database session across multiple `SimpleDBIO` actions, make sure to
use `withPinnedSession` or `transactionally` accordingly (see [above](#transactions)).
