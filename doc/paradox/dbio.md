Database I/O Actions
====================

This chapter explains how to execute, compose, and control `SlickAction` values.

In this chapter:

- Execute actions with `run` (materialized) or `stream` (streaming).
- Compose actions with sequencing and error-handling combinators.
- Control transaction/session behavior with `transactionally` and `withPinnedSession`.
- Drop down to JDBC when needed with `SimpleDBIO`.

Anything that you can execute on a database, whether it is a getting the result of a query
(`myQuery.result`), creating a table (`myTable.schema.create`), inserting data
(`myTable += item`) or something else, is an instance of
@scaladoc[SlickAction](slick.dbio.SlickAction), parameterized by the result type it will produce when you execute it.

*Database I/O Actions* can be combined with several different combinators (see the
@scaladoc[SlickAction class](slick.dbio.SlickAction) and @scaladoc[SlickAction object](slick.dbio.SlickAction$), which is also
available under the alias `DBIO`, for details), but they will always be executed strictly sequentially and (at least
conceptually) in a single database session.

In most cases you will want to use the type aliases @scaladoc[DBIO](slick.dbio.package#DBIO[+R]:DBIO[R])
and @scaladoc[StreamingDBIO](slick.dbio.package#StreamingDBIO[+R,+T]:StreamingDBIO[R,T]) for non-streaming and
streaming Database I/O Actions. They omit the optional *effect types* supported by @scaladoc[SlickAction](slick.dbio.SlickAction).

@@@ note

In the code examples below we assume the following imports:

@@snip [Connection.scala](../code/Connection.scala) { #imports }

If you're new to Slick, please start with the  @ref:[Getting Started](gettingstarted.md) page.

@@@

Executing Database Actions
------------------------------

Start here if you want to run a query or action and get results back.

`SlickAction`s can be executed either with the goal of producing a fully materialized result or streaming
data back from the database.

### Materialized

You can use `run` to execute a `SlickAction` on a Database and produce a materialized result. This can
be, for example, a scalar query result (`myTable.length.result`), a collection-valued query
result (`myTable.to[Set].result`), or any other action. Every `SlickAction` supports this mode of
execution.

Execution of the action starts in the background when `run` is called. The calling thread is not blocked. The
materialized result is returned as an `F[R]` value (for example `IO[Seq[User]]`) that completes
asynchronously and can be composed with the rest of your effect-based program:

@@snip [Connection.scala](../code/Connection.scala) { #materialize }

### Streaming

Collection-valued queries also support streaming results. In this case, the actual collection type
is ignored and elements are streamed directly from the result set through the streaming type of the
selected Slick facade.

Execution of the `SlickAction` does not start until the stream is consumed.

Repeated-consumption semantics depend on the selected facade stream type. See the facade-specific
Scaladoc for `slick.cats.Database` and `slick.zio.Database`.

Stream elements are signaled as soon as they become available in the streaming part of the `SlickAction`. The end of
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

This section covers how to build larger workflows from smaller actions.

`SlickAction`s describe sequences of individual actions to execute in strictly sequential order on
one database session (at least conceptually), therefore the most commonly used combinators deal with
sequencing. Since a `SlickAction` eventually results in a `Success` or `Failure`, its combinators
have to distinguish between successful and failed executions. Unless specifically noted, all
combinators only apply to successful actions. Any failure aborts the sequence of execution.

### Sequential Execution

The simplest combinator is @scaladoc[DBIO.seq](slick.dbio.SlickAction$#seq[E%3C:Effect](SlickAction[NoStream,E,_]*):SlickAction[NoStream,E,Unit])
which takes a varargs list of actions to run in sequence, discarding their return value. If you
need the return value, you can use @scaladoc[andThen](slick.dbio.SlickAction#andThen[R2,S2%3C:NoStream,E2%3C:Effect](SlickAction[S2,E2,R2]):SlickAction[S2,EwithE2,R2])
to combine two actions and keep the result of the second one. If you need both return values of two
actions, there is the @scaladoc[zip](slick.dbio.SlickAction#zip[R2,E2%3C:Effect](SlickAction[NoStream,E2,R2]):SlickAction[NoStream,EwithE2,(R,R2)])
combinator. For getting all result values from a sequence of actions (of compatible types), use
@scaladoc[DBIO.sequence](slick.dbio.SlickAction$#sequence[R,M[+_]%3C:TraversableOnce[_],E%3C:Effect](M[SlickAction[NoStream,E,R]])(CanBuildFrom[M[SlickAction[NoStream,E,R]],R,M[R]]):SlickAction[NoStream,E,M[R]]).
All these combinators work with pre-existing `SlickAction`s which are composed eagerly:

@@snip [DBIOCombinators.scala](../code/DBIOCombinators.scala) { #combinators1 }

If an action depends on a previous action in the sequence, you have to compute it on the fly with
@scaladoc[flatMap](slick.dbio.SlickAction#flatMap[R2,S2%3C:NoStream,E2%3C:Effect]((R)=%3ESlickAction[S2,E2,R2]):SlickAction[S2,EwithE2,R2])
or @scaladoc[map](slick.dbio.SlickAction#map[R2]((R)=%3ER2):SlickAction[NoStream,E,R2]).
These two methods plus @scaladoc[filter](slick.dbio.SlickAction#filter((R)=%3EBoolean):SlickAction[NoStream,E,R])
enable the use of *for comprehensions* for action sequencing.

Similar to @scaladoc[DBIO.sequence](slick.dbio.SlickAction$#sequence[R,M[+_]%3C:TraversableOnce[_],E%3C:Effect](M[SlickAction[NoStream,E,R]])(CanBuildFrom[M[SlickAction[NoStream,E,R]],R,M[R]]):SlickAction[NoStream,E,M[R]])
for upfront composition, there is @scaladoc[DBIO.fold](slick.dbio.SlickAction$#fold[T,E%3C:Effect](Seq[SlickAction[NoStream,E,T]],T)((T,T)=%3ET):SlickAction[NoStream,E,T])
for working with sequences of actions and composing them based on the previous result.

### Error Handling

You can use @scaladoc[andFinally](slick.dbio.SlickAction#andFinally[E2%3C:Effect](SlickAction[NoStream,E2,_]):SlickAction[S,EwithE2,R])
to perform a cleanup action, no matter whether the previous action succeeded or failed. This is similar to using
`try ... finally ...` in imperative Scala code. A more flexible version of
@scaladoc[andFinally](slick.dbio.SlickAction#andFinally[E2%3C:Effect](SlickAction[NoStream,E2,_]):SlickAction[S,EwithE2,R])
is @scaladoc[cleanUp](slick.dbio.SlickAction#cleanUp[E2%3C:Effect]((Option[Throwable])=%3ESlickAction[NoStream,E2,_],Boolean):SlickAction[S,EwithE2,R]).
It lets you transform the failure and decide how to fail the resulting action if both the original
one and the cleanup failed.

@@@ note
For even more flexible error handling use
@scaladoc[asTry](slick.dbio.SlickAction#asTry:SlickAction[NoStream,E,Try[R]])
and @scaladoc[failed](slick.dbio.SlickAction#failed:SlickAction[NoStream,E,Throwable]). Unlike with
@scaladoc[andFinally](slick.dbio.SlickAction#andFinally[E2%3C:Effect](SlickAction[NoStream,E2,_]):SlickAction[S,EwithE2,R])
and @scaladoc[cleanUp](slick.dbio.SlickAction#cleanUp[E2%3C:Effect]((Option[Throwable])=%3ESlickAction[NoStream,E2,_],Boolean):SlickAction[S,EwithE2,R])
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

You can lift any CE3 effect `F[R]` into an action with @scaladoc[DBIO.from](slick.dbio.SlickAction$#from[F[_],R](F[R]):SlickAction[NoStream,Effect,R]).
`DBIO.liftF` is an alias for `DBIO.from`. This allows an `IO` (or any other `F[_]: Async` value)
to be used in an action sequence:

```scala
val action: DBIO[String] = for {
  id <- DBIO.from(IO(java.util.UUID.randomUUID().toString))
  _  <- users += User(id, "Alice")
} yield id
```

A pre-existing value or failure can be converted with
@scaladoc[DBIO.successful](slick.dbio.SlickAction$#successful[R](R):SlickAction[NoStream,Effect,R])
and @scaladoc[DBIO.failed](slick.dbio.SlickAction$#failed(Throwable):SlickAction[NoStream,Effect,Nothing]), respectively.

### Debugging

The @scaladoc[named](slick.dbio.SlickAction#named(String):SlickAction[S,E,R]) combinator names an
action. This name can be seen in debug logs if you enable the `slick.basic.BasicBackend.action`  @ref:[logger](config.md#logging).

### Transactions and Pinned Sessions {#transactions}

When executing a `SlickAction` which is composed of several smaller actions, Slick acquires sessions from the connection
pool and releases them again as needed so that a session is not kept in use unnecessarily while waiting for the result
of a non-database computation (e.g. the function passed to
@scaladoc[flatMap](slick.dbio.SlickAction#flatMap[R2,S2%3C:NoStream,E2%3C:Effect]((R)=%3ESlickAction[S2,E2,R2]):SlickAction[S2,EwithE2,R2])
that determines the next action to run). You can use
@scaladoc[withPinnedSession](slick.dbio.SlickAction#withPinnedSession:SlickAction[S,E,R]) to force the use of a single
session, keeping the existing session open even when waiting for non-database computations.

All @scaladoc[SlickAction combinators](slick.dbio.SlickAction) which combine database actions without any non-database
computations in between (e.g.
@scaladoc[andThen](slick.dbio.SlickAction#andThen[R2,S2%3C:NoStream,E2%3C:Effect](SlickAction[S2,E2,R2]):SlickAction[S2,EwithE2,R2])
or
@scaladoc[zip](slick.dbio.SlickAction#zip[R2,E2%3C:Effect](SlickAction[NoStream,E2,R2]):SlickAction[NoStream,EwithE2,(R,R2)])
applied to two database computations) can fuse these actions for more efficient execution, with the side-effect that
the fused action runs inside a single session, even without `withPinnedSession`.

There is a related combinator called
@scaladoc[transactionally](slick.jdbc.JdbcActionComponent$JdbcActionExtensionMethods#transactionally:SlickAction[S,EwithTransactional,R])
to force the use of a transaction. This guarantees that the entire `SlickAction` that is executed will
either succeed or fail atomically. Without it, all database actions run in auto-commit mode. The use of a transaction
always implies a pinned session.

An overload
@scaladoc[transactionally(ti)](slick.jdbc.JdbcActionComponent$JdbcActionExtensionMethods#transactionally(TransactionIsolation):SlickAction[S,EwithTransactional,R])
accepts a `TransactionIsolation` level:

```scala
action.transactionally(TransactionIsolation.Serializable)
```

@@snip [Connection.scala](../code/Connection.scala) { #transaction }

@@@ warning
Warning: Failure is not guaranteed to be atomic *at the level of an individual* `SlickAction` that is wrapped with
`transactionally`, so you need to be careful where you apply error recovery combinators. An actual database
transaction is only created and committed or rolled back for the outermost `transactionally` action. Nested
`transactionally` actions simply execute inside the existing transaction without additional savepoints.
@@@

@@@ note
**Cancellation guarantee**: in Slick 4 a transaction is rolled back not only on error but also on
fiber cancellation. This guarantee was not possible with `Future`-based execution.
@@@

### Rollbacks

In case you want to force a rollback, you can return `DBIO.failed` within a `SlickAction`.

@@snip [Connection.scala](../code/Connection.scala) { #rollback }

JDBC Interoperability
---------------------

Use this when you need JDBC functionality that is not directly modeled in Slick.

In order to drop down to the JDBC level for functionality that is not available in Slick, you can
use a `SimpleDBIO` action which is run on a database thread and gets access to the JDBC `Connection`:

@@snip [Connection.scala](../code/Connection.scala) { #simpleaction }

If you need to access state of the database session across multiple `SimpleDBIO` actions, make sure to
use `withPinnedSession` or `transactionally` accordingly (see [above](#transactions)).
