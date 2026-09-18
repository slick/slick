Database I/O Actions
====================

This chapter explains how to execute, compose, and control `DBIOAction` values.

In this chapter:

- Execute actions with `run` (materialized) or `stream` (streaming).
- Compose actions with sequencing and error-handling combinators.
- Control transaction/session behavior with `transactionally` and `withPinnedSession`.
- Drop down to JDBC when needed with `SimpleDBIO`.

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
The alias @scaladoc[DBIOEffect](slick.dbio.package#DBIOEffect[-E%3C:Effect,+R]:DBIOEffect[E,R]) keeps the effect type
and omits only the streaming type; see @ref:[Cats Type Classes](#cats-type-classes).

@@@ note

In the code examples below we assume the following imports:

@@snip [Connection.scala](../code/Connection.scala) { #imports }

If you're new to Slick, please start with the  @ref:[Getting Started](gettingstarted.md) page.

@@@

Executing Database Actions
------------------------------

Start here if you want to run a query or action and get results back.

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
is ignored and elements are streamed directly from the result set through the streaming type of the
selected Slick facade.

Execution of the `DBIOAction` does not start until the stream is consumed.

Repeated-consumption semantics depend on the selected facade stream type. See the facade-specific
Scaladoc for `slick.cats.Database` and `slick.zio.Database`.

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

This section covers how to build larger workflows from smaller actions.

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

### Cats Type Classes {#cats-type-classes}

Database I/O Actions have [cats](https://typelevel.org/cats/) instances: `MonadError[F, Throwable]`, and a
`Semigroup` or `Monoid` for actions whose result type has one. The instances live in the companion objects of
@scaladoc[DBIOBase](slick.dbio.DBIOBase) and @scaladoc[DBIOAction](slick.dbio.DBIOAction$), so they are in
implicit scope and need no import besides the cats syntax itself (`import cats.syntax.all._`).

A cats type class takes a unary type constructor `F[_]`, and there are two ways to view an action as one:

- @scaladoc[DBIOEffect](slick.dbio.package#DBIOEffect[-E%3C:Effect,+R]:DBIOEffect[E,R])`[E, R]` is
  `DBIOAction[R, NoStream, E]` with the result type last, so that `DBIOEffect[E, *]` is an `F[_]` for every
  effect `E`. `DBIO[R]` is `DBIOEffect[Effect.All, R]`. The non-streaming combinators of `DBIOAction` and its
  companion (`map`, `zip`, `asTry`, `DBIO.sequence`, `DBIO.successful`, ...) declare their results as
  `DBIOEffect`, and `toAction` gives any action, including the profile-specific ones returned by the lifted API,
  the `DBIOEffect` type with its effect.
- @scaladoc[DBIOBase](slick.dbio.DBIOBase)`[E, R]` is the supertype of every `DBIOAction[R, S, E]` with the
  effect and result type in that order and no streaming type. On Scala 3 a `DBIOAction` unifies with an `F[_]`
  as `DBIOBase[E, *]`, so any action, including streaming and profile-specific ones, works with cats syntax as
  it is and keeps its effect: `List(1, 2).traverse(i => coffees += (s"Coffee $i", 1.0))` is a
  `DBIOBase[Effect.Write, List[Int]]`. Scala 2 does not unify a `DBIOAction` type with `DBIOBase` on its own;
  there `DBIOBase` is used when spelled out, and profile actions go through `toAction`.

The following works on every Scala version:

@@snip [DBIOCombinators.scala](../code/DBIOCombinators.scala) { #cats }

A `DBIOBase` produced by a cats combinator is accepted wherever a `DBIOAction` is expected, including
`db.run`, the arguments of Slick's combinators and the right-hand side of a for comprehension generator; `map`,
`flatMap`, `andThen` and `>>` are members of `DBIOBase` and intersect the effects as usual. Extension methods such
as `transactionally` need the explicit `toAction`, because Scala does not chain two implicit conversions.

@@@ note
cats type classes are invariant in `F`, so the receiver of a cats combinator fixes the effect:
`readAction.flatTap(_ => writeAction)` does not compile. Use Slick's `flatMap` or `>>`, which intersect the
effects, or widen the receiver to `DBIO`, which accepts every effect: `(readAction: DBIO[Int]).flatTap(...)`.
Where no receiver fixes `F` (`tupled`, `mapN`, `*>`, `sequence` over a mixed list) Scala 3 infers the effect
intersection; Scala 2 needs the `DBIO` ascription there as well.
@@@

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
accepts a `TransactionIsolation` level:

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

@@@ note
**Cancellation guarantee**: in Slick 4 a transaction is rolled back not only on error but also on
fiber cancellation. This guarantee was not possible with `Future`-based execution.
@@@

### Rollbacks

In case you want to force a rollback, you can return `DBIO.failed` within a `DBIOAction`.

@@snip [Connection.scala](../code/Connection.scala) { #rollback }

JDBC Interoperability
---------------------

Use this when you need JDBC functionality that is not directly modeled in Slick.

In order to drop down to the JDBC level for functionality that is not available in Slick, you can
use a `SimpleDBIO` action which is run on a database thread and gets access to the JDBC `Connection`:

@@snip [Connection.scala](../code/Connection.scala) { #simpleaction }

If you need to access state of the database session across multiple `SimpleDBIO` actions, make sure to
use `withPinnedSession` or `transactionally` accordingly (see [above](#transactions)).
