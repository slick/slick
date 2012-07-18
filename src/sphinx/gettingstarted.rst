Getting Started
===============

The easiest way of setting up a SLICK application is by starting with the
`SLICK Examples`_ project. You can build and run this project by following the
instructions in its README file.

Dependencies
------------

Let's take a closer look at what's happening in that project. First of all,
you need to add SLICK and the embedded databases or drivers for external
databases to your project. If you are using sbt_, you do this in your
main ``build.sbt`` file::

  libraryDependencies ++= List(
    "com.typesafe" % "slick" %% "0.10.0-SNAPSHOT",
    "org.slf4j" % "slf4j-nop" % "1.6.4",
    "com.h2database" % "h2" % "1.3.166"
  )

SLICK uses SLF4J_ for its own debug logging so you also need to add an SLF4J
implementation. Here we are using ``slf4j-nop`` to disable logging. You have
to replace this with a real logging framework like Logback_ if you want to see
log output.

First Example
-------------

`FirstExample.scala`_ contains a self-contained application that you can run.
It starts off with some imports:

.. includecode:: code/FirstExample.scala#imports

Since we are using H2_ as our database system, we need to import features
from SLICK's ``H2Driver``. A driver's ``simple`` object contains all commonly
needed imports from the driver and other parts of SLICK such as
:doc:`session handling <session>`. The only extra import we use is the
``threadLocalSession``. This simplifies the session handling by attaching a
session to the current thread so you do not have to pass it around on your own
(or at least assign it to an implicit variable).

In the body of the application we create a ``Database`` object which specifies
how to connect to a database, and then immediately open a session, running all
code within the following block inside that session:

.. includecode:: code/FirstExample.scala#setup

In a Java SE environment, database sessions are usually created by connecting
to a JDBC URL using a JDBC driver class (see the JDBC driver's documentation
for the correct URL syntax). If you are only using
:doc:`plain SQL queries <sql>`, nothing more is required, but when SLICK is
generating SQL code for you (using :doc:`plain Scala queries <queryable>` or
the :doc:`query language <ql>`), you need to make sure to use a matching
SLICK driver (in our case the ``H2Driver`` import above).

We are using the :doc:`query language <ql>` in this application, so we have to
write ``Table`` objects for our database tables:

.. includecode:: code/FirstExample.scala#tables

All columns get a name (usually in camel case for Scala and upper case with
underscores for SQL) and a Scala type (from which the SQL type can be derived
automatically). Make sure to define them with ``def`` and not with ``val``.
The table object also needs a Scala name, SQL name and type. The type of the
table must match the type of the special ``*`` projection. In simple cases
this is a tuple of all columns but more complex mappings are possible.
