Database I/O Actions
====================

Anything that you can execute on a database, whether it is a getting the result of a query
(``myQuery.result``), creating a table (``myTable.schema.create``), inserting data
(``myTable += item``) or something else, is an instance of
:api:`slick.dbio.DBIOAction`, parameterized by the result type it will produce when you
execute it.

*Database I/O Actions* can be combined with several different combinators (see the
:api:`DBIOAction class <slick.dbio.DBIOAction>` and :api:`DBIO object <slick.dbio.DBIO$>`
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

Transactions and Pinned Sessions
________________________________

When executing a ``DBIOAction`` which is composed of several smaller actions, Slick acquires sessions from
the connection pool and releases them again as needed so that a session is not kept in use
unnecessarily while waiting for the result from a non-database computation (e.g. the function
passed to
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
