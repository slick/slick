.. index:: Activator, template

Getting Started
###############

The easiest way to get started is with a working application in Typesafe Activator_. The following
templates are created by the Slick team, with updated versions being made for new Slick releases:

* To learn the basics of Slick, start with the `Hello Slick template`_. It contains an extended
  version of the tutorial and code from this page.

* The `Slick Plain SQL Queries template`_ shows you how to do SQL queries with Slick.

* The `Slick Multi-DB Patterns template`_ shows you how to write Slick applications that can use
  different database systems and how to use custom database functions in Slick queries.

* The `Slick TestKit Example template`_ shows you how to use Slick TestKit to test your own Slick drivers.

There are more Slick templates created by the community, as well as versions of our own templates for other
Slick releases. You can find `all Slick templates <https://typesafe.com/activator/templates#filter:slick>`_
on the Typesafe web site.

.. index:: Maven, sbt, artifacts, build, dependency

.. _dependencies:

Adding Slick to Your Project
============================

To include Slick in an existing project use the library published on Maven Central.  For sbt projects add the
following to your build definition - ``build.sbt`` or ``project/Build.scala``:

.. parsed-literal::
  libraryDependencies ++= Seq(
    "com.typesafe.slick" %% "slick" % "|release|",
    "org.slf4j" % "slf4j-nop" % "1.6.4"
  )

For Maven projects add the following to your ``<dependencies>`` (make sure to use the correct Scala
version prefix, ``_2.10`` or ``_2.11``, to match your project's Scala version):

.. parsed-literal::
  <dependency>
    <groupId>com.typesafe.slick</groupId>
    <artifactId>slick_2.10</artifactId>
    <version>\ |release|\ </version>
  </dependency>
  <dependency>
    <groupId>org.slf4j</groupId>
    <artifactId>slf4j-nop</artifactId>
    <version>1.6.4</version>
  </dependency>

.. index:: logging, SLF4j

Slick uses SLF4J_ for its own debug logging so you also need to add an SLF4J
implementation. Here we are using ``slf4j-nop`` to disable logging. You have
to replace this with a real logging framework like Logback_ if you want to see
log output.

The Reactive Streams API is pulled in automatically as a transitive dependency.

If you want to use Slick's connection pool support for HikariCP_, you need to add
the ``slick-hikaricp`` module as a dependency in the same way as shown for ``slick``
above. It will automatically provide a compatible version of HikariCP as a transitive
dependency.

Quick Introduction
==================

.. note::
   The rest of this chapter is based on the `Hello Slick template`_. The prefered
   way of reading this introduction is in Activator_, where you can edit and run the code
   directly while reading the tutorial.

To use Slick you first need to import the API for the database you will be using, like:

.. includecode:: code/FirstExample.scala#imports

Since we are using H2_ as our database system, we need to import features
from Slick's ``H2Driver``. A driver's ``api`` object contains all commonly
needed imports from the driver and other parts of Slick such as
:doc:`database handling <database>`.

Slick's API is fully asynchronous and runs database call in a separate thread pool. For running
user code in composition of ``DBIOAction`` and ``Future`` values, we import the global
``ExecutionContext``. When using Slick as part of a larger application (e.g. with Play_ or
Akka_) the framework may provide a better alternative to this default ``ExecutionContext``.

.. _gettingstarted-dbconnection:

Database Configuration
----------------------

In the body of the application we create a ``Database`` object which specifies how to connect to a
database. In most cases you will want to configure database connections with `Typesafe Config`_ in
your ``application.conf``, which is also used by Play_ and Akka_ for their configuration:

.. includecode:: resources/application.conf#h2mem1

For the purpose of this example we disable the connection pool (there is no point in using one for
an embedded in-memory database) and request a keep-alive connection (which ensures that the
database does not get dropped while we are using it). The database can be easily instantiated from
the configuration like this:

.. includecode:: code/FirstExample.scala#setup

.. note::
   A ``Database`` object usually manages a thread pool and a connection pool. You should always
   shut it down properly when it is no longer needed (unless the JVM process terminates anyway).

Schema
------

Before we can write Slick queries, we need to describe a database schema with ``Table`` row classes
and ``TableQuery`` values for our tables. You can either use the :doc:`code generator <code-generation>`
to automatically create them for your database schema or you can write them by hand:

.. includecode:: code/FirstExample.scala#tables

All columns get a name (usually in camel case for Scala and upper case with
underscores for SQL) and a Scala type (from which the SQL type can be derived
automatically).
The table object also needs a Scala name, SQL name and type. The type argument
of the table must match the type of the special ``*`` projection. In simple
cases this is a tuple of all columns but more complex mappings are possible.

The ``foreignKey`` definition in the ``coffees`` table ensures that the
``supID`` field can only contain values for which a corresponding ``id``
exists in the ``suppliers`` table, thus creating an *n to one* relationship:
A ``Coffees`` row points to exactly one ``Suppliers`` row but any number
of coffees can point to the same supplier. This constraint is enforced at the
database level.

Populating the Database
-----------------------

The connection to the embedded H2 database engine provides us with an empty
database. Before we can execute queries, we need to create the database schema
(consisting of the ``coffees`` and ``suppliers`` tables) and insert some test
data:

.. includecode:: code/FirstExample.scala#create

The ``TableQuery``'s ``ddl`` method creates ``DDL`` (data definition language) objects
with the database-specific code for creating and dropping tables and other
database entities. Multiple ``DDL`` values can be combined with ``++`` to
allow all entities to be created and dropped in the correct order, even when
they have circular dependencies on each other.

Inserting the tuples of data is done with the ``+=`` and ``++=`` methods,
similar to how you add data to mutable Scala collections.

The ``create``, ``+=`` and ``++=`` methods return an ``Action`` which can be executed on a database
at a later time to produce a result. There are several different combinators for combining multiple
Actions into sequences, yielding another Action. Here we use the simplest one, ``Action.seq``, which
can concatenate any number of Actions, discarding the return values (i.e. the resulting Action
produces a result of type ``Unit``). We then execute the setup Action asynchronously with
``db.run``, yielding a ``Future[Unit]``.

.. note::
   Database connections and transactions are managed automatically by Slick. By default
   connections are acquired and released on demand and used in *auto-commit* mode. In this mode we
   have to populate the ``suppliers`` table first because the ``coffees`` data can only refer to valid
   supplier IDs. We could also use an explicit transaction bracket encompassing all these statements
   (``db.run(setup.transactionally)``). Then the order would not matter because the constraints are
   only enforced at the end when the transaction is committed.

Querying
--------

The simplest kind of query iterates over all the data in a table:

.. includecode:: code/FirstExample.scala#readall

This corresponds to a ``SELECT * FROM COFFEES`` in SQL (except that the ``*``
is the table's ``*`` projection we defined earlier and not whatever the
database sees as ``*``). The type of the values we get in the loop is,
unsurprisingly, the type parameter of ``Coffees``.

Let's add a *projection* to this basic query. This is written in Scala with
the ``map`` method or a *for comprehension*:

.. includecode:: code/FirstExample.scala#projection

The output will be the same: For each row of the table, all columns get
converted to strings and concatenated into one tab-separated string. The
difference is that all of this now happens inside the database engine, and
only the resulting concatenated string is shipped to the client. Note that we
avoid Scala's ``+`` operator (which is already heavily overloaded) in favor of
``++`` (commonly used for sequence concatenation). Also, there is no automatic
conversion of other argument types to strings. This has to be done explicitly
with the type conversion method ``asColumnOf``.

This time we also use `Reactive Streams`_ to get a streaming result from the
database and print the elements as they come in instead of materializing the
whole result set upfront.

Joining and filtering tables is done the same way as when working with Scala
collections:

.. includecode:: code/FirstExample.scala#join

.. warning::
   Note the use of ``===`` instead of ``==`` for comparing two values for equality and ``=!=``
   instead of ``!=`` for inequality. This is necessary because these operators are already defined
   (with unsuitable types and semantics) on the base type ``Any``, so they cannot be replaced by
   extension methods. The other comparison operators are the same as in standard Scala code:
   ``<``, ``<=``, ``>=``, ``>``.

The generator expression ``suppliers if s.id === c.supID`` follows the
relationship established by the foreign key ``Coffees.supplier``. Instead of
repeating the join condition here we can use the foreign key directly:

.. includecode:: code/FirstExample.scala#fkjoin
