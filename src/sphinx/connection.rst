Connections / Transactions
================================

You can write queries anywhere in your program. When you want to execute them
you need a database connection.

Database connection
------------------------------------

You can tell Slick how to connect to the database of your choice by creating
a :api:`scala.slick.session.Database` object, which encapsulates the
information. There are several
:api:`factory methods <scala.slick.session.Database$>`
you can use depending on what you have.

..  This also determines which kind of database you are using.

.. TODO: add after adding getting started guide Make sure you have imported  the
.. :ref:`required dependencies <getting-starget-dependencies>` and imported the
.. correct :ref:`Slick driver <getting-starget-driver>`.



Using a JDBC URL
^^^^^^^^^^^^^^^^^^^^

You can provide a JDBC URL to
:api:`forURL <scala.slick.session.Database$@forURL(String,String,String,Properties,String):Database>`.
(see your database's JDBC driver's documentation for the correct URL syntax).

.. includecode:: code/Connection.scala#forURL

Here we are connecting to a new, empty in-memory H2 database called ``test1``
and keep it resident until the JVM ends (``DB_CLOSE_DELAY=-1``, which is H2
specific).

.. TODO: mention that you have to import a matching driver

Using a DataSource
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

You can provide a :javaapi:`DataSource <javax/sql/DataSource>` object to
:api:`forDataSource <scala.slick.session.Database$@forDataSource(DataSource):Database>`.
If you got it  from the connection pool of your application framework, this
plugs the pool into Slick.

.. includecode:: code/Connection.scala#forDataSource

When you later :ref:`create a Session <session-handling>`, a connection is
acquired from the pool and when the Session is closed it is returned to the
pool.

Using a JNDI Name
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

If you are using :wikipedia:`JNDI` you can provide a JNDI name to
:api:`forName <scala.slick.session.Database$@forName(String):Database>`
under which a
:javaapi:`DataSource <javax/sql/DataSource>` object can be looked up.

.. includecode:: code/Connection.scala#forName

.. _session-handling:

Session handling
--------------------------------------------

Now you have a :api:`scala.slick.session.Database` object and can use it to open database
connections, which Slick encapsulates in :api:`scala.slick.session.Session`
objects.

.. _session-scope:

Automatically closing Session scope
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The :api:`scala.slick.session.Database` object's
:api:`withSession <scala.slick.session.Database@withSession[T]((Session)⇒T):T>`
method creates a
:api:`scala.slick.session.Session`, passes it to a given function and closes it
afterwards. If you use a connection pool, closing the
:api:`scala.slick.session.Session` returns the connection to the pool.

.. includecode:: code/Connection.scala#withSession

You can see how we are able to already define the query outside of the
:api:`withSession <scala.slick.session.Database@withSession[T]((Session)⇒T):T>`
scope. Only the methods actually executing the query in the database require a
:api:`scala.slick.session.Session`. Here we use the
:api:`list <scala.slick.jdbc.Invoker@list(P)(Session):List[R]>`
method to execute the query
and return the results as a :scalaapi:`scala.collection.immutable.List`. (The
executing methods are made available via implicit conversions).

Note that by default a database session is in **auto-commit** mode. Each call to
the database like
:api:`insert <scala.slick.driver.BasicInvokerComponent$InsertInvoker@insert(U)(Session):RetOne>`
or :api:`insertAll <scala.slick.driver.BasicInvokerComponent$InsertInvoker@insertAll(U*)(Session):RetMany>`
executes atomically (i.e. it succeeds or fails completely).
To bundle several statements use :ref:`transactions`.

**Be careful:** If the :api:`scala.slick.session.Session` object escapes the
:api:`withSession <scala.slick.session.Database@withSession[T]((Session)⇒T):T>`
scope, it has already been closed and is invalid. It can escape in several ways,
which should be avoided, e.g. as state of a closure (if you use a
:scalaapi:`Future <scala.concurrent.package@Future[T](⇒T)(ExecutionContext):Future[T]>`
inside a :api:`withSession <scala.slick.session.Database@withSession[T]((Session)⇒T):T>`
scope for example), by assigning the session to a var, by returning the session
as the return value of the withSession scope or else.

Implicit Session
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

By marking the :api:`scala.slick.session.Session` as implicit you can avoid
having to pass it to the executing methods explicitly.

.. includecode:: code/Connection.scala#withSession-implicit

This is optional of course. Use it if you think it makes your code cleaner.

.. _transactions:

Transactions
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

You can use the :api:`scala.slick.session.Session` object's
:api:`withTransaction <scala.slick.session.Session@withTransaction[T](⇒T):T>`
method to create a transaction when you need one. The block passed to it
is executed in a single transaction. If an exception is thrown, Slick rolls
back the transaction at the end of the block. You can force the rollback at the end by calling
:api:`rollback <scala.slick.session.Session@rollback():Unit>` anywhere within the block.
Be aware that
Slick only rolls back database operations, not the effects of other Scala code.

.. includecode:: code/Connection.scala#transaction

If you don't have a :api:`scala.slick.session.Session` yet you can use the
:api:`scala.slick.session.Database` object's
:api:`withTransaction <scala.slick.session.Database@withTransaction[T]((Session)⇒T):T>`
method as a shortcut.

.. includecode:: code/Connection.scala#independentTransaction

Manual Session handling
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

This is not recommended, but if you have to, you can handle the lifetime of a
:api:`scala.slick.session.Session` manually.

.. includecode:: code/Connection.scala#manual-session

Passing sessions around
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

You can write re-useable functions to help with Slick queries. They mostly do
not need a :api:`scala.slick.session.Session` as they just produce query
fragments or assemble queries. If you want to execute queries inside of them
however, they need a :api:`scala.slick.session.Session`. You can either put it
into the function signature and pass it as a (possibly implicit) argument. Or
you can bundle several such methods into a class, which stores the session to
reduce boilerplate code:

.. includecode:: code/Connection.scala#helpers

Dynamically scoped sessions
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

You usually do not want to keep sessions open for very long but open and close
them quickly when needed. As shown above you may use a
:ref:`session scope <session-scope>` or :ref:`transaction scope <transactions>`
with an implicit session argument every time you need to execute some queries.

Alternatively you can save a bit of boilerplate code by putting

.. includecode:: code/Connection.scala#dynamicSession-import

at the top of your file and then using a session scope or transaction scope
without a session argument.

.. includecode:: code/Connection.scala#withSession-empty

:api:`dynamicSession <scala.slick.session.Database$@dynamicSession:Session>` is an
implicit def that returns a valid :api:`scala.slick.session.Session` if a
:api:`withDynSession <scala.slick.session.Database@withDynSession[T](⇒T):T>`
or :api:`withDynTransaction :<scala.slick.session.Database@withDynTransaction[T](⇒T):T>`
scope is open somewhere on the current call stack.

Be careful, if you import
:api:`dynamicSession <scala.slick.session.Database$@dynamicSession:Session>`
and try to execute a query outside of a
:api:`withDynSession <scala.slick.session.Database@withDynSession[T](⇒T):T>`
or :api:`withDynTransaction <scala.slick.session.Database@withDynTransaction[T](⇒T):T>`
scope, you will get a runtime exception. So you sacrifice some static safety for less
boilerplate. :api:`dynamicSession <scala.slick.session.Database$@dynamicSession:Session>`
internally uses :scalaapi:`scala.util.DynamicVariable`, which implements
dynamically scoped variables and in turn uses Java's
:javaapi:`InheritableThreadLocal <java/lang/InheritableThreadLocal>`. Be aware
of the consequences regarding static safety and thread safety.

.. TODO: explain how session relates to connection

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
