Database I/O Actions
====================

Anything that you can execute on a database, whether it is a getting the result of a query
(``myQuery.result``), creating a table (``myTable.schema.create``), inserting data
(``myTable += item``) or something else, is an instance of
:api:`slick.dbio.DBIOAction`, parameterized by the result type it will produce when you
execute it.

*Database I/O Actions* can be combined with several different combinators (see the
:api:`DBIOAction class <slick.dbio.DBIOAction>` and :api:`DBIO object <slick.dbio.DBIOAction$>`
for details), but they will always be executed strictly sequentially and (at least conceptually) in a
single database session.

In most cases you will want to use the type aliases :api:`DBIO <slick.dbio.package@DBIO[+R]:DBIO[R]>`
and :api:`StreamingDBIO <slick.dbio.package@StreamingDBIO[+R,+T]:StreamingDBIO[R,T]>` for non-streaming and
streaming Database I/O Actions. They omit the optional *effect types* supported by :api:`slick.dbio.DBIOAction`.

.. index::
   pair: execute; Action

.. _executing-actions:

Executing Database I/O Actions
------------------------------

``DBIOAction``\ s can be executed either with the goal of producing a fully materialized result or streaming
data back from the database.

.. index:: materialize

Materialized
____________

You can use ``run`` to execute a ``DBIOAction`` on a Database and produce a materialized result. This can
be, for example, a scalar query result (``myTable.length.result``), a collection-valued query
result (``myTable.to[Set].result``), or any other action. Every ``DBIOAction`` supports this mode of
execution.

Execution of the action starts when ``run`` is called, and the materialized result is returned as a
``Future`` which is completed asynchronously as soon as the result is available:

.. includecode:: code/Connection.scala#materialize

.. index:: stream

Streaming
_________

Collection-valued queries also support streaming results. In this case, the actual collection type
is ignored and elements are streamed directly from the result set through a `Reactive Streams`_
``Publisher``, which can be processed and consumed by `Akka Streams`_.

Execution of the ``DBIOAction`` does not start until a ``Subscriber`` is attached to the stream. Only a single
``Subscriber`` is supported, and any further attempts to subscribe again will fail. Stream elements are
signaled as soon as they become available in the streaming part of the ``DBIOAction``. The end of the
stream is signaled only after the entire action has completed. For example, when streaming inside
a transaction and all elements have been delivered successfully, the stream can still fail
afterwards if the transaction cannot be committed.

.. includecode:: code/Connection.scala#stream

When streaming a JDBC result set, the next result page will be buffered in the background if the
Subscriber is not ready to receive more data, but all elements are signaled synchronously and the
result set is not advanced before synchronous processing is finished. This allows synchronous
callbacks to low-level JDBC values like ``Blob`` which depend on the state of the result set. The
convenience method ``mapResult`` is provided for this purpose:

.. includecode:: code/Connection.scala#streamblob

.. index::
   pair: session; pinned
   single: transaction
.. _transactions:

Composing Database I/O Actions
------------------------------

``DBIOAction``\ s describe sequences of individual actions to execute in strictly sequential order on
one database session (at least conceptually), therefore the most commonly used combinators deal with
sequencing. Since a ``DBIOAction`` eventually results in a ``Success`` or ``Failure``, its combinators,
just like the ones on ``Future``, have to distinguish between successful and failed executions. Unless
specifically noted, all combinators only apply to successful actions. Any failure will abort the
sequence of execution and result in a failed ``Future`` or *Reactive Stream*.

Sequential Execution
____________________

The simplest combinator is :api:`DBIO.seq <slick.dbio.DBIOAction$@seq[E<:Effect](DBIOAction[_,NoStream,E]*):DBIOAction[Unit,NoStream,E]>`
which takes a varargs list of actions to run in sequence, discarding their return value. If you
need the return value, you can use :api:`andThen <slick.dbio.DBIOAction@andThen[R2,S2<:NoStream,E2<:Effect](DBIOAction[R2,S2,E2]):DBIOAction[R2,S2,EwithE2]>`
to combine two actions and keep the result of the second one. If you need both return values of two
actions, there is the :api:`zip <slick.dbio.DBIOAction@zip[R2,E2<:Effect](DBIOAction[R2,NoStream,E2]):DBIOAction[(R,R2),NoStream,EwithE2]>`
combinator. For getting all result values from a sequence of actions (of compatible types), use
:api:`DBIO.sequence <slick.dbio.DBIOAction$@sequence[R,M[+_]<:TraversableOnce[_],E<:Effect](M[DBIOAction[R,NoStream,E]])(CanBuildFrom[M[DBIOAction[R,NoStream,E]],R,M[R]]):DBIOAction[M[R],NoStream,E]>`.
All these combinators work with pre-existing ``DBIOAction``\ s which are composed eagerly.

If an action depends on a previous action in the sequence, you have to compute it on the fly with
:api:`flatMap <slick.dbio.DBIOAction@flatMap[R2,S2<:NoStream,E2<:Effect]((R)⇒DBIOAction[R2,S2,E2])(ExecutionContext):DBIOAction[R2,S2,EwithE2]>`
or :api:`map <slick.dbio.DBIOAction@map[R2]((R)⇒R2)(ExecutionContext):DBIOAction[R2,NoStream,E]>`.
These two methods plus :api:`filter <slick.dbio.DBIOAction@filter((R)⇒Boolean)(ExecutionContext):DBIOAction[R,NoStream,E]>`
enable the use of *for comprehensions* for action sequencing. Since they take function
arguments, they also require an implicit ``ExecutionContext`` on which to run the function. This
way Slick ensures that no non-database code is run on the database thread pool.

.. note::
   You should prefer the less flexible methods without an ``ExecutionContext`` where possible. The
   resulting actions can be executed more efficiently.

Similar to :api:`DBIO.sequence <slick.dbio.DBIOAction$@sequence[R,M[+_]<:TraversableOnce[_],E<:Effect](M[DBIOAction[R,NoStream,E]])(CanBuildFrom[M[DBIOAction[R,NoStream,E]],R,M[R]]):DBIOAction[M[R],NoStream,E]>`
for upfront composition, there is :api:`DBIO.fold <slick.dbio.DBIOAction$@fold[T,E<:Effect](Seq[DBIOAction[T,NoStream,E]],T)((T,T)⇒T)(ExecutionContext):DBIOAction[T,NoStream,E]>`
for working with sequences of actions and composing them based on the previous result.

Error Handling
______________

You can use :api:`andFinally <slick.dbio.DBIOAction@andFinally[E2<:Effect](DBIOAction[_,NoStream,E2]):DBIOAction[R,S,EwithE2]>`
to perform a cleanup action, no matter whether the previous action succeeded or failed. This is similar to using
``try ... finally ...`` in imperative Scala code. A more flexible version of
:api:`andFinally <slick.dbio.DBIOAction@andFinally[E2<:Effect](DBIOAction[_,NoStream,E2]):DBIOAction[R,S,EwithE2]>`
is :api:`cleanUp <slick.dbio.DBIOAction@cleanUp[E2<:Effect]((Option[Throwable])⇒DBIOAction[_,NoStream,E2],Boolean)(ExecutionContext):DBIOAction[R,S,EwithE2]>`.
It lets you transform the failure and decide how to fail the resulting action if both the original
one and the cleanup failed.

.. note::
   For even more flexible error handling use :api:`asTry <slick.dbio.DBIOAction@asTry:DBIOAction[Try[R],NoStream,E]>`
   and :api:`failed <slick.dbio.DBIOAction@failed:DBIOAction[Throwable,NoStream,E]>`. Unlike with
   :api:`andFinally <slick.dbio.DBIOAction@andFinally[E2<:Effect](DBIOAction[_,NoStream,E2]):DBIOAction[R,S,EwithE2]>`
   and :api:`cleanUp <slick.dbio.DBIOAction@cleanUp[E2<:Effect]((Option[Throwable])⇒DBIOAction[_,NoStream,E2],Boolean)(ExecutionContext):DBIOAction[R,S,EwithE2]>`
   the resulting actions cannot be used for streaming.

Primitives
__________

You can convert a ``Future`` into an action with :api:`DBIO.from <slick.dbio.DBIOAction$@from[R](Future[R]):DBIOAction[R,NoStream,Effect]>`.
This allows the result of the ``Future`` to be used in an action sequence. A pre-existing value or
failure can be converted with :api:`DBIO.successful <slick.dbio.DBIOAction$@successful[R](R):DBIOAction[R,NoStream,Effect]>`
and :api:`DBIO.failed <slick.dbio.DBIOAction$@failed(Throwable):DBIOAction[Nothing,NoStream,Effect]>`, respectively.

Debugging
_________

The :api:`named <slick.dbio.DBIOAction@named(String):DBIOAction[R,S,E]>` combinator names an
action. This name can be seen in debug logs if you enable the
``slick.backend.DatabaseComponent.action`` logger.

Transactions and Pinned Sessions
________________________________

When executing a ``DBIOAction`` which is composed of several smaller actions, Slick acquires sessions from
the connection pool and releases them again as needed so that a session is not kept in use
unnecessarily while waiting for the result from a non-database computation (e.g. the function passed to
:api:`flatMap <slick.dbio.DBIOAction@flatMap[R2,S2<:NoStream,E2<:Effect]((R)⇒DBIOAction[R2,S2,E2])(ExecutionContext):DBIOAction[R2,S2,EwithE2]>`
that determines the next Action to run). All :api:`DBIOAction combinators <slick.dbio.DBIOAction>`
which combine two database actions without any non-database computations in between (e.g.
:api:`andThen <slick.dbio.DBIOAction@andThen[R2,S2<:NoStream,E2<:Effect](DBIOAction[R2,S2,E2]):DBIOAction[R2,S2,EwithE2]>`
or :api:`zip <slick.dbio.DBIOAction@zip[R2,E2<:Effect](DBIOAction[R2,NoStream,E2]):DBIOAction[(R,R2),NoStream,EwithE2]>`)
can fuse these actions for more efficient execution, with the side-effect that the fused action
runs inside a single session. You can use
:api:`withPinnedSession <slick.dbio.DBIOAction@withPinnedSession:DBIOAction[R,S,E]>` to force the
use of a single session, keeping the existing session open even when waiting for non-database
computations.

There is a similar combinator called
:api:`transactionally <slick.driver.JdbcActionComponent$JdbcActionExtensionMethods@transactionally:DBIOAction[R,S,EwithTransactional]>`
to force the use of a transaction. This guarantees that the entire ``DBIOAction`` that is executed will
either succeed or fail atomically.

.. warning::
   Failure is not guaranteed to be atomic *at the level of an individual* ``DBIOAction`` that is wrapped with
   ``transactionally``, so you should not apply error recovery combinators at that point. An actual database
   transaction is inly created and committed / rolled back for the outermost ``transactionally`` action.

.. includecode:: code/Connection.scala#transaction

.. index:: JDBC
.. _jdbc-interop:

JDBC Interoperability
---------------------

In order to drop down to the JDBC level for functionality that is not available in Slick, you can
use a ``SimpleDBIO`` action which is run on a database thread and gets access to the JDBC ``Connection``:

.. includecode:: code/Connection.scala#simpleaction
