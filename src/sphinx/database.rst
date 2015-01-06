Databases & Actions
===================

Anything that you can execute on a database, whether it is a getting the result of a query
("``myQuery.result``"), creating a table ("``myTable.schema.create``"), inserting data
("``myTable += item``") or something else, is an instance of
:api:`scala.slick.action.EffectfulAction`, parameterized by the result type it will produce when you
execute it.

Actions can be combined with several different combinators (see the
:api:`EffectfulAction class <scala.slick.action.EffectfulAction>` and :api:`Action object <scala.slick.action.EffectfulAction>` for
details), but they will always be executed strictly sequentially and (at least conceptually) in a
single database session.

.. index::
   pair: database; configuration
   pair: database; pool

Database configuration
----------------------

You can tell Slick how to connect to the JDBC database of your choice by
creating a :api:`Database <scala.slick.jdbc.JdbcBackend@Database:Database>` object,
which encapsulates the information. There are several
:api:`factory methods <scala.slick.jdbc.JdbcBackend$DatabaseFactoryDef>`
on `scala.slick.jdbc.JdbcBackend.Database` that you can use depending on what
connection data you have available.

Using Typesafe Config
_____________________

The prefered way to configure database connections is through `Typesafe Config`_ in your
``application.conf``, which is also used by Play_ and Akka_ for their configuration.

.. includecode:: resources/application.conf#mydb

Such a configuration can be loaded with `Database.forConfig` (see the
:api:`API documentation <scala.slick.jdbc.JdbcBackend$DatabaseFactoryDef@forConfig(String,Config,Driver):Database>`
of this method for details on the configuration parameters).

.. includecode:: code/Connection.scala#forConfig

.. index:: URL

Using a JDBC URL
________________

You can provide a JDBC URL to
:api:`forURL <scala.slick.jdbc.JdbcBackend$DatabaseFactoryDef@forURL(String,String,String,Properties,String,AsyncExecutor,Boolean):DatabaseDef>`.
(see your database's JDBC driver's documentation for the correct URL syntax).

.. includecode:: code/Connection.scala#forURL

Here we are connecting to a new, empty, in-memory H2 database called ``test1``
and keep it resident until the JVM ends (``DB_CLOSE_DELAY=-1``, which is H2
specific).

.. index:: DataSource

Using a DataSource
__________________

You can provide a :javaapi:`DataSource <javax/sql/DataSource>` object to
:api:`forDataSource <scala.slick.jdbc.JdbcBackend$DatabaseFactoryDef@forDataSource(DataSource,AsyncExecutor):DatabaseDef>`.
If you got it  from the connection pool of your application framework, this
plugs the pool into Slick.

.. includecode:: code/Connection.scala#forDataSource

.. index:: JNDI

Using a JNDI Name
_________________

If you are using :wikipedia:`JNDI` you can provide a JNDI name to
:api:`forName <scala.slick.jdbc.JdbcBackend$DatabaseFactoryDef@forName(String,AsyncExecutor):DatabaseDef>`
under which a
:javaapi:`DataSource <javax/sql/DataSource>` object can be looked up.

.. includecode:: code/Connection.scala#forName

.. index::
   pair: thread; pool

Database thread pool
--------------------

Every ``Database`` contains an :api:`scala.slick.util.AsyncExecutor` that manages the thread pool
for asynchronous execution of database Actions. Its size is the main parameter to tune for the best
performance of the ``Database`` object. It should be set to the value that you would use for the
size of the *connection pool* in a traditional, blocking application (see `About Pool Sizing`_
in the HikariCP_ documentation for further information). When using
:api:`Database.forConfig <scala.slick.jdbc.JdbcBackend$DatabaseFactoryDef@forConfig(String,Config,Driver):Database>`,
the thread pool is configured directly in the external configuration file together with the connection
parameters. If you use any other factory method to get a ``Database``, you can either use a default
configuration or specify a custom AsyncExecutor:

.. includecode:: code/Connection.scala#forURL2

.. index::
   pair: connection; pool

Connection pools
----------------

When using a connection pool (which is always recommended in production environments) the *minimum*
size of the *connection pool* should also be set to at least the same size. The *maximum* size of
the *connection pool* can be set much higher than in a blocking application. Any connections beyond
the size of the *thread pool* will only be used when other connections are required to keep a
database session open (e.g. while waiting for the result from an asynchronous computation in the
middle of a transaction) but are not actively doing any work on the database.

Note that reasonable defaults for the connection pool sizes are calculated from the thread pool
size when using
:api:`Database.forConfig <scala.slick.jdbc.JdbcBackend$DatabaseFactoryDef@forConfig(String,Config,Driver):Database>`.

Slick uses *prepared* statements wherever possible but it does not cache them on its own. You
should therefore enable prepared statement caching in the connection pool's configuration.

.. index::
   pair: execute; Action

Executing Actions
-----------------

Actions can be executed either with the goal of producing a fully materialized result or streaming
data back from the database.

.. index:: materialize

Materialized
____________

You can use ``run`` to execute an Action on a Database and produce a materialized result. This can
be, for example, a scalar query result ("``myTable.length.result``"), a collection-valued query
result ("``myTable.to[Set].result``"), or any other Action. Every Action supports this mode of
execution.

Execution of the Action starts when ``run`` is called, and the materialized result is returned as a
``Future`` which is completed asynchronously as soon as the result is available:

.. includecode:: code/Connection.scala#materialize

.. index:: stream

Streaming
_________

Collection-valued queries also support streaming results. In this case, the actual collection type
is ignored and elements are streamed directly from the result set through a `Reactive Streams`_
``Publisher``, which can be processed and consumed by `Akka Streams`_.

Execution of the Action does not start until a Subscriber is attached to the stream. Only a single
Subscriber is supported, and any further attempts to subscribe again will fail. Stream elements are
signaled as soon as they become available in the streaming part of the Action. The end of the
stream is signaled only after the entire Action has completed. For example, when streaming inside
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

When executing an Action that is composed of several smaller Actions, Slick acquires sessions from
the connection pool and releases them again as needed so that a session is not kept in use
unnecessarily while waiting for the result from a non-database computation (e.g. the function
passed to
:api:`flatMap <scala.slick.action.EffectfulAction@flatMap[E2<:Effect,R2,S2<:NoStream]((R)⇒EffectfulAction[E2,R2,S2])(ExecutionContext):EffectfulAction[EwithE2,R2,S2]>`
that determines the next Action to run). All :api:`Action combinators <scala.slick.action.EffectfulAction>`
which combine two database Actions without any non-database computations in between (e.g.
:api:`andThen <scala.slick.action.EffectfulAction@andThen[E2<:Effect,R2,S2<:NoStream](EffectfulAction[E2,R2,S2]):EffectfulAction[EwithE2,R2,S2]>`
or :api:`zip <scala.slick.action.EffectfulAction@zip[E2<:Effect,R2](EffectfulAction[E2,R2,NoStream]):EffectfulAction[EwithE2,(R,R2),NoStream]>`)
can fuse these Actions for more efficient execution, with the side-effect that the fused Action
runs inside a single session. You can use
:api:`withPinnedSession <scala.slick.action.EffectfulAction@withPinnedSession:EffectfulAction[E,R,S]>` to force the
use of a single session, keeping the existing session open even when waiting for non-database
computations.

There is a similar combinator
:api:`transactionally <scala.slick.driver.JdbcActionComponent$JdbcActionExtensionMethods@transactionally:EffectfulAction[EwithTransactional,R,S]>`
to force the use of a transaction. This guarantees that the entire Action that is executed will
either succeed or fail atomically.  Note that failure is not guaranteed to be atomic at the level
of an individual Action that is wrapped with ``.transactionally``, so you should not apply error
recovery combinators at that point.

.. includecode:: code/Connection.scala#transaction

.. index:: JDBC
.. _jdbc-interop:

JDBC Interoperability
---------------------

In order to drop down to the JDBC level for functionality that is not available in Slick, you can
use a ``SimpleAction`` which is run on a database thread and gets access to the JDBC ``Connection``:

.. includecode:: code/Connection.scala#simpleaction
