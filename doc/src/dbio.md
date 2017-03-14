Database I/O Actions
====================

Anything that you can execute on a database, whether it is a getting the result of a query
(`myQuery.result`), creating a table (`myTable.schema.create`), inserting data
(`myTable += item`) or something else, is an instance of
<api:slick.dbio.DBIOAction>, parameterized by the result type it will produce when you execute it.

*Database I/O Actions* can be combined with several different combinators (see the
[DBIOAction class](api:slick.dbio.DBIOAction) and [DBIOAction object](api:slick.dbio.DBIOAction$), which is also
available under the alias `DBIO`, for details), but they will always be executed strictly sequentially and (at least
conceptually) in a single database session.

In most cases you will want to use the type aliases [DBIO](api:slick.dbio.package@DBIO[+R]:DBIO[R])
and [StreamingDBIO](api:slick.dbio.package@StreamingDBIO[+R,+T]:StreamingDBIO[R,T]) for non-streaming and
streaming Database I/O Actions. They omit the optional *effect types* supported by <api:slick.dbio.DBIOAction>.

Executing Database I/O Actions {index="execute,Action;Action,execute"}
------------------------------

`DBIOAction`s can be executed either with the goal of producing a fully materialized result or streaming
data back from the database.

### Materialized {index=materialize}

You can use `run` to execute a `DBIOAction` on a Database and produce a materialized result. This can
be, for example, a scalar query result (`myTable.length.result`), a collection-valued query
result (`myTable.to[Set].result`), or any other action. Every `DBIOAction` supports this mode of
execution.

Execution of the action starts in the background when `run` is called. The calling thread is not blocked. The
materialized result is returned as a [Future][Scala Futures] which is completed asynchronously as soon as the result
is available:

```scala src=../code/Connection.scala#materialize
```

### Streaming {index=stream}

Collection-valued queries also support streaming results. In this case, the actual collection type
is ignored and elements are streamed directly from the result set through a [Reactive Streams]
`Publisher`, which can be processed and consumed by [Akka Streams].

Execution of the `DBIOAction` does not start until a `Subscriber` is attached to the stream. If multiple
`Subscriber`s subscribe to the same `Publisher`, each one triggers an *independent execution* of the
action.

Stream elements are signaled as soon as they become available in the streaming part of the `DBIOAction`. The end of
the stream is signaled only after the *entire action* has completed. For example, when streaming inside a transaction
and all elements have been delivered successfully, the stream can still fail afterwards if the transaction cannot be
committed. The `Subscriber` is notififed of this failure.

```scala src=../code/Connection.scala#stream
```

When streaming a JDBC result set, the next result page will be buffered in the background if the
Subscriber is not ready to receive more data, but all elements are signaled synchronously and the
result set is not advanced before synchronous processing is finished. This allows synchronous
callbacks to low-level JDBC values like `Blob` which depend on the state of the result set. The
convenience method `mapResult` is provided for this purpose:

```scala src=../code/Connection.scala#streamblob
```

> {.note}
> Note: Some database systems may require session parameters to be set in a certain way to support streaming without
> caching all data at once in memory on the client side. For example, [PostgreSQL] requires both
> `.withStatementParameters(rsType = ResultSetType.ForwardOnly, rsConcurrency = ResultSetConcurrency.ReadOnly, fetchSize = n)`
> (with the desired page size `n`) and `.transactionally` for proper streaming.

Composing Database I/O Actions {index="session,pinned;pinned,session;transaction"}
------------------------------

`DBIOAction`s describe sequences of individual actions to execute in strictly sequential order on
one database session (at least conceptually), therefore the most commonly used combinators deal with
sequencing. Since a `DBIOAction` eventually results in a `Success` or `Failure`, its combinators,
just like the ones on `Future`, have to distinguish between successful and failed executions. Unless
specifically noted, all combinators only apply to successful actions. Any failure will abort the
sequence of execution and result in a failed `Future` or *Reactive Stream*.

The available DBIO combinators are a purely functional subset of Future combinators. You should be
familiar with [working with Scala Futures][Scala Futures] before diving into DBIO combinators. Since the
result of a database action is usually a Future, this knowledge is required anyway for composing database
results and other asynchronous code.

### Sequential Execution

The simplest combinator is [DBIO.seq](api:slick.dbio.DBIOAction$@seq[E%3C:Effect](DBIOAction[_,NoStream,E]*):DBIOAction[Unit,NoStream,E])
which takes a varargs list of actions to run in sequence, discarding their return value. If you
need the return value, you can use <api:slick.dbio.DBIOAction@andThen[R2,S2%3C:NoStream,E2%3C:Effect](DBIOAction[R2,S2,E2]):DBIOAction[R2,S2,EwithE2]>
to combine two actions and keep the result of the second one. If you need both return values of two
actions, there is the <api:slick.dbio.DBIOAction@zip[R2,E2%3C:Effect](DBIOAction[R2,NoStream,E2]):DBIOAction[(R,R2),NoStream,EwithE2]>
combinator. For getting all result values from a sequence of actions (of compatible types), use
[DBIO.sequence](api:slick.dbio.DBIOAction$@sequence[R,M[+_]%3C:TraversableOnce[_],E%3C:Effect](M[DBIOAction[R,NoStream,E]])(CanBuildFrom[M[DBIOAction[R,NoStream,E]],R,M[R]]):DBIOAction[M[R],NoStream,E]).
All these combinators work with pre-existing `DBIOAction`s which are composed eagerly:

```scala src=../code/DBIOCombinators.scala#combinators1
```

If an action depends on a previous action in the sequence, you have to compute it on the fly with
<api:slick.dbio.DBIOAction@flatMap[R2,S2%3C:NoStream,E2%3C:Effect]((R)=%3EDBIOAction[R2,S2,E2])(ExecutionContext):DBIOAction[R2,S2,EwithE2]>
or <api:slick.dbio.DBIOAction@map[R2]((R)=%3ER2)(ExecutionContext):DBIOAction[R2,NoStream,E]>.
These two methods plus <api:slick.dbio.DBIOAction@filter((R)=%3EBoolean)(ExecutionContext):DBIOAction[R,NoStream,E]>
enable the use of *for comprehensions* for action sequencing. Since they take function
arguments, they also require an implicit `ExecutionContext` on which to run the function. This
way Slick ensures that no non-database code is run on the database thread pool. This ExecutionContext should be
provided by your application or framework (e.g. [Akka] or [Play]).

> {.note}
> Note: You should prefer the less flexible methods without an `ExecutionContext` where possible. The
> resulting actions can be executed more efficiently.

Similar to [DBIO.sequence](api:slick.dbio.DBIOAction$@sequence[R,M[+_]%3C:TraversableOnce[_],E%3C:Effect](M[DBIOAction[R,NoStream,E]])(CanBuildFrom[M[DBIOAction[R,NoStream,E]],R,M[R]]):DBIOAction[M[R],NoStream,E])
for upfront composition, there is [DBIO.fold](<api:slick.dbio.DBIOAction$@fold[T,E%3C:Effect](Seq[DBIOAction[T,NoStream,E]],T)((T,T)=%3ET)(ExecutionContext):DBIOAction[T,NoStream,E]>)
for working with sequences of actions and composing them based on the previous result.

### Error Handling

You can use <api:slick.dbio.DBIOAction@andFinally[E2%3C:Effect](DBIOAction[_,NoStream,E2]):DBIOAction[R,S,EwithE2]>
to perform a cleanup action, no matter whether the previous action succeeded or failed. This is similar to using
`try ... finally ...` in imperative Scala code. A more flexible version of
<api:slick.dbio.DBIOAction@andFinally[E2%3C:Effect](DBIOAction[_,NoStream,E2]):DBIOAction[R,S,EwithE2]>
is <api:slick.dbio.DBIOAction@cleanUp[E2%3C:Effect]((Option[Throwable])=%3EDBIOAction[_,NoStream,E2],Boolean)(ExecutionContext):DBIOAction[R,S,EwithE2]>.
It lets you transform the failure and decide how to fail the resulting action if both the original
one and the cleanup failed.

> {.note}
> Note: For even more flexible error handling use <api:slick.dbio.DBIOAction@asTry:DBIOAction[Try[R],NoStream,E]>
> and <api:slick.dbio.DBIOAction@failed:DBIOAction[Throwable,NoStream,E]>. Unlike with
> <api:slick.dbio.DBIOAction@andFinally[E2%3C:Effect](DBIOAction[_,NoStream,E2]):DBIOAction[R,S,EwithE2]>
> and <api:slick.dbio.DBIOAction@cleanUp[E2%3C:Effect]((Option[Throwable])=%3EDBIOAction[_,NoStream,E2],Boolean)(ExecutionContext):DBIOAction[R,S,EwithE2]>
> the resulting actions cannot be used for streaming.

### Primitives

You can convert a `Future` into an action with [DBIO.from](api:slick.dbio.DBIOAction$@from[R](Future[R]):DBIOAction[R,NoStream,Effect]).
This allows the result of the `Future` to be used in an action sequence. A pre-existing value or
failure can be converted with [DBIO.successful](api:slick.dbio.DBIOAction$@successful[R](R):DBIOAction[R,NoStream,Effect])
and [DBIO.failed](api:slick.dbio.DBIOAction$@failed(Throwable):DBIOAction[Nothing,NoStream,Effect]), respectively.

### Debugging

The <api:slick.dbio.DBIOAction@named(String):DBIOAction[R,S,E]> combinator names an
action. This name can be seen in debug logs if you enable the `slick.basic.BasicBackend.action` [logger](config.md#logging).

### Transactions and Pinned Sessions {#transactions}

When executing a `DBIOAction` which is composed of several smaller actions, Slick acquires sessions from
the connection pool and releases them again as needed so that a session is not kept in use
unnecessarily while waiting for the result of a non-database computation (e.g. the function passed to
<api:slick.dbio.DBIOAction@flatMap[R2,S2%3C:NoStream,E2%3C:Effect]((R)=%3EDBIOAction[R2,S2,E2])(ExecutionContext):DBIOAction[R2,S2,EwithE2]>
that determines the next action to run). You can use <api:slick.dbio.DBIOAction@withPinnedSession:DBIOAction[R,S,E]> to
force the use of a single session, keeping the existing session open even when waiting for non-database computations.

All [DBIOAction combinators](api:slick.dbio.DBIOAction) which combine database actions without any non-database
computations in between (e.g.
<api:slick.dbio.DBIOAction@andThen[R2,S2%3C:NoStream,E2%3C:Effect](DBIOAction[R2,S2,E2]):DBIOAction[R2,S2,EwithE2]>
or <api:slick.dbio.DBIOAction@zip[R2,E2%3C:Effect](DBIOAction[R2,NoStream,E2]):DBIOAction[(R,R2),NoStream,EwithE2]>
applied to two database computations) can fuse these actions for more efficient execution, with the side-effect that
the fused action runs inside a single session, even without `withPinnedSession`.

There is a related combinator called
<api:slick.jdbc.JdbcActionComponent$JdbcActionExtensionMethods@transactionally:DBIOAction[R,S,EwithTransactional]>
to force the use of a transaction. This guarantees that the entire `DBIOAction` that is executed will
either succeed or fail atomically. Without it, all database actions run in auto-commit mode. The use of a transaction
always implies a pinned session.

```scala src=../code/Connection.scala#transaction
```

> {.warning}
> Warning: Failure is not guaranteed to be atomic *at the level of an individual* `DBIOAction` that is wrapped with
> `transactionally`, so you need to be careful where you apply error recovery combinators. An actual database
> transaction is only created and committed or rolled back for the outermost `transactionally` action. Nested
> `transactionally` actions simply execute inside the existing transaction without additional savepoints.

### Rollbacks

In case you want to force a rollback, you can return `DBIO.failed` within a `DBIOAction`.

```scala src=../code/Connection.scala#rollback
```

JDBC Interoperability {index=JDBC}
---------------------

In order to drop down to the JDBC level for functionality that is not available in Slick, you can
use a `SimpleDBIO` action which is run on a database thread and gets access to the JDBC `Connection`:

```scala src=../code/Connection.scala#simpleaction
```

If you need to access state of the database session across multiple `SimpleDBIO` actions, make sure to
use `withPinnedSession` or `transactionally` accordingly (see [above](#transactions)).
