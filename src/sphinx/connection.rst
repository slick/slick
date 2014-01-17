Connections / Transactions
================================

You can write queries anywhere in your program. When you want to execute them
you need a database connection.

.. index::
   pair: database; connection

Database connection
-------------------

You can tell Slick how to connect to the JDBC database of your choice by
creating a :api:`Database <scala.slick.jdbc.JdbcBackend@Database:Database>` object,
which encapsulates the information. There are several
:api:`factory methods <scala.slick.jdbc.JdbcBackend$DatabaseFactoryDef>`
on `scala.slick.jdbc.JdbcBackend.Database` that you can use depending on what
connection data you have available.

..  This also determines which kind of database you are using.

.. TODO: add after adding getting started guide Make sure you have imported  the
.. :ref:`required dependencies <getting-starget-dependencies>` and imported the
.. correct :ref:`Slick driver <getting-starget-driver>`.

.. index:: URL

Using a JDBC URL
________________

You can provide a JDBC URL to
:api:`forURL <scala.slick.jdbc.JdbcBackend$DatabaseFactoryDef@forURL(String,String,String,Properties,String):DatabaseDef>`.
(see your database's JDBC driver's documentation for the correct URL syntax).

.. includecode:: code/Connection.scala#forURL

Here we are connecting to a new, empty, in-memory H2 database called ``test1``
and keep it resident until the JVM ends (``DB_CLOSE_DELAY=-1``, which is H2
specific).

.. TODO: mention that you have to import a matching driver

.. index:: DataSource

Using a DataSource
__________________

You can provide a :javaapi:`DataSource <javax/sql/DataSource>` object to
:api:`forDataSource <scala.slick.jdbc.JdbcBackend$DatabaseFactoryDef@forDataSource(DataSource):DatabaseDef>`.
If you got it  from the connection pool of your application framework, this
plugs the pool into Slick.

.. includecode:: code/Connection.scala#forDataSource

When you later :ref:`create a Session <session-handling>`, a connection is
acquired from the pool and when the Session is closed it is returned to the
pool.

.. index:: JNDI

Using a JNDI Name
_________________

If you are using :wikipedia:`JNDI` you can provide a JNDI name to
:api:`forName <scala.slick.jdbc.JdbcBackend$DatabaseFactoryDef@forName(String):DatabaseDef>`
under which a
:javaapi:`DataSource <javax/sql/DataSource>` object can be looked up.

.. includecode:: code/Connection.scala#forName

.. index:: session, connection
.. _session-handling:

Session handling
----------------

Now you have a :api:`Database <scala.slick.jdbc.JdbcBackend@Database:Database>` object
and you can use it to open database connections, which Slick encapsulates in
:api:`Session <scala.slick.jdbc.JdbcBackend$SessionDef>` objects.

.. _session-scope:

Automatically closing Session scope
___________________________________

The :api:`Database <scala.slick.jdbc.JdbcBackend@Database:Database>` object's
:api:`withSession <scala.slick.jdbc.JdbcBackend$DatabaseDef@withSession[T]((Session)⇒T):T>`
method creates a
:api:`Session <scala.slick.jdbc.JdbcBackend$SessionDef>`, passes it to a given function and closes it
afterwards. If you use a connection pool, closing the
:api:`Session <scala.slick.jdbc.JdbcBackend$SessionDef>` returns the connection to the pool.

.. includecode:: code/Connection.scala#withSession

You can see how we are able to already define the query outside of the
:api:`withSession <scala.slick.jdbc.JdbcBackend$DatabaseDef@withSession[T]((Session)⇒T):T>`
scope. Only the methods actually executing the query in the database require a
:api:`Session <scala.slick.jdbc.JdbcBackend$SessionDef>`. Here we use the
:api:`list <scala.slick.jdbc.Invoker@list(SessionDef):List[R]>`
method to execute the query
and return the results as a :scalaapi:`scala.collection.immutable.List`. (The
executing methods are made available via implicit conversions).

Note that by default a database session is in **auto-commit** mode. Each call to
the database like
:api:`insert <scala.slick.driver.JdbcInsertInvokerComponent$BaseInsertInvoker@insert(U)(SessionDef):SingleInsertResult>`
or :api:`insertAll <scala.slick.driver.JdbcInsertInvokerComponent$BaseInsertInvoker@insertAll(U*)(SessionDef):MultiInsertResult>`
executes atomically (i.e. it succeeds or fails completely).
To bundle several statements use :ref:`transactions`.

**Be careful:** If the :api:`Session <scala.slick.jdbc.JdbcBackend$SessionDef>` object escapes the
:api:`withSession <scala.slick.jdbc.JdbcBackend$DatabaseDef@withSession[T]((Session)⇒T):T>`
scope, it has already been closed and is invalid. It can escape in several ways,
which should be avoided, e.g. as state of a closure (if you use a
:scalaapi:`Future <scala.concurrent.package@Future[T](⇒T)(ExecutionContext):Future[T]>`
inside a :api:`withSession <scala.slick.jdbc.JdbcBackend$DatabaseDef@withSession[T]((Session)⇒T):T>`
scope for example), by assigning the session to a var, by returning the session
as the return value of the withSession scope or else.

.. index::
   pair: session; implicit

Implicit Session
________________

By marking the :api:`Session <scala.slick.jdbc.JdbcBackend$SessionDef>` as implicit you can avoid
having to pass it to the executing methods explicitly.

.. includecode:: code/Connection.scala#withSession-implicit

This is optional of course. Use it if you think it makes your code cleaner.

.. index:: transaction
.. _transactions:

Transactions
____________

You can use the :api:`Session <scala.slick.jdbc.JdbcBackend$SessionDef>` object's
:api:`withTransaction <scala.slick.jdbc.JdbcBackend$SessionDef@withTransaction[T](⇒T):T>`
method to create a transaction when you need one. The block passed to it
is executed in a single transaction. If an exception is thrown, Slick rolls
back the transaction at the end of the block. You can force the rollback at the end by calling
:api:`rollback <scala.slick.jdbc.JdbcBackend$SessionDef@rollback():Unit>` anywhere within the block.
Be aware that
Slick only rolls back database operations, not the effects of other Scala code.

.. includecode:: code/Connection.scala#transaction

If you don't have a :api:`Session <scala.slick.jdbc.JdbcBackend$SessionDef>` yet you can use the
:api:`Database <scala.slick.jdbc.JdbcBackend@Database:Database>` object's
:api:`withTransaction <scala.slick.jdbc.JdbcBackend$DatabaseDef@withTransaction[T]((Session)⇒T):T>`
method as a shortcut.

.. includecode:: code/Connection.scala#independentTransaction

.. index::
   single: session; manual

Manual Session handling
_______________________

This is not recommended, but if you have to, you can handle the lifetime of a
:api:`Session <scala.slick.jdbc.JdbcBackend$SessionDef>` manually.

.. includecode:: code/Connection.scala#manual-session

Passing sessions around
_______________________

You can write re-useable functions to help with Slick queries. They mostly do
not need a :api:`Session <scala.slick.jdbc.JdbcBackend$SessionDef>` as they just produce query
fragments or assemble queries. If you want to execute queries inside of them
however, they need a :api:`Session <scala.slick.jdbc.JdbcBackend$SessionDef>`. You can either put it
into the function signature and pass it as a (possibly implicit) argument. Or
you can bundle several such methods into a class, which stores the session to
reduce boilerplate code:

.. includecode:: code/Connection.scala#helpers

.. index::
   pair: session; dynamic
   single: thread-local

Dynamically scoped sessions
___________________________

You usually do not want to keep sessions open for very long but open and close
them quickly when needed. As shown above you may use a
:ref:`session scope <session-scope>` or :ref:`transaction scope <transactions>`
with an implicit session argument every time you need to execute some queries.

Alternatively you can save a bit of boilerplate code by putting

.. includecode:: code/Connection.scala#dynamicSession-import

at the top of your file and then using a session scope or transaction scope
without a session argument.

.. includecode:: code/Connection.scala#withSession-empty

:api:`dynamicSession <scala.slick.jdbc.JdbcBackend$DatabaseFactoryDef@dynamicSession:Session>` is an
implicit def that returns a valid :api:`Session <scala.slick.jdbc.JdbcBackend$SessionDef>` if a
:api:`withDynSession <scala.slick.jdbc.JdbcBackend$DatabaseDef@withDynSession[T](⇒T):T>`
or :api:`withDynTransaction :<scala.slick.jdbc.JdbcBackend$DatabaseDef@withDynTransaction[T](⇒T):T>`
scope is open somewhere on the current call stack.

Be careful, if you import
:api:`dynamicSession <scala.slick.jdbc.JdbcBackend$DatabaseFactoryDef@dynamicSession:Session>`
and try to execute a query outside of a
:api:`withDynSession <scala.slick.jdbc.JdbcBackend$DatabaseDef@withDynSession[T](⇒T):T>`
or :api:`withDynTransaction <scala.slick.jdbc.JdbcBackend$DatabaseDef@withDynTransaction[T](⇒T):T>`
scope, you will get a runtime exception. So you sacrifice some static safety for less
boilerplate. :api:`dynamicSession <scala.slick.jdbc.JdbcBackend$DatabaseFactoryDef@dynamicSession:Session>`
internally uses :scalaapi:`scala.util.DynamicVariable`, which implements
dynamically scoped variables and in turn uses Java's
:javaapi:`InheritableThreadLocal <java/lang/InheritableThreadLocal>`. Be aware
of the consequences regarding static safety and thread safety.

.. TODO: explain how session relates to connection

.. index::
   single: connection; pool
   single: pool

Connection Pools
----------------

Slick does not provide a connection pool implementation of its own. When you
run a managed application in some container (e.g. JEE or Spring), you should
generally use the connection pool provided by the container. For stand-alone
applications you can use an external pool implementation like DBCP_, c3p0_
or BoneCP_.

Note that Slick uses *prepared* statements wherever possible but it does not
cache them on its own. You should therefore enable prepared statement caching
in the connection pool's configuration and select a sufficiently large pool
size.

.. index:: JDBC
.. _jdbc-interop:

JDBC interoperability
---------------------
To access features not available in Slick directly it can be useful to drop down to JDBC level.

You can access the underlying :javaapi:`JDBC Connection <java/sql/Connection>` of a Slick :api:`Session <scala.slick.jdbc.JdbcBackend$SessionDef>` using the :api:`conn <scala.slick.jdbc.JdbcBackend$SessionDef@conn:Connection>` method.

:api:`Session <scala.slick.jdbc.JdbcBackend$SessionDef>`'s
:api:`withPreparedInsertStatement <scala.slick.jdbc.JdbcBackend$SessionDef@withPreparedInsertStatement[T](String,Array[String])((PreparedStatement)⇒T):T>`,
:api:`withPreparedStatement <scala.slick.jdbc.JdbcBackend$SessionDef@withPreparedStatement[T](String,ResultSetType,ResultSetConcurrency,ResultSetHoldability)((PreparedStatement)⇒T):T>`,
:api:`withStatement <scala.slick.jdbc.JdbcBackend$SessionDef@withStatement[T](ResultSetType,ResultSetConcurrency,ResultSetHoldability)((Statement)⇒T):T>` methods allow you to create automatically closing :javaapi:`JDBC Statements <java/sql/Statement>`.
