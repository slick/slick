.. index:: configuration, options

Configuration
=============

In addition to configuring your :doc:`database connections <database>` in ``application.conf`` you can also set some
global options in this file or through system properties (see the `Typesafe Config`_ documentation for details).
These are the available options and their default values as defined in Slick's own ``reference.conf``:

.. includecode:: ../main/resources/reference.conf
   :language: sh

.. index:: logging, slf4j

.. _logging:

Logging
=======

Slick uses SLF4J_ for logging. How to configure loggers and appenders depends on the actual logging framework that you
use in your application. The following Logback_ configuration is used internally for testing and debugging of Slick. It
contains a list of all loggers that are used by Slick for debug output:

.. includecode:: ../../../common-test-resources/logback.xml
   :language: xml

.. note::
   Messages on WARNING and ERROR levels may also be emitted by loggers that are not explicitly mentioned in this
   configuration.

You should generally enable logging for the root package ``slick`` at level ``INFO`` (currently unused)
or ``WARNING``. Debug logging should only be enabled selectively for individual loggers, otherwise you will get a huge
amount of log output.

The following loggers are particularly interesting:

``slick.basic.BasicBackend.action``
   Shows the execution of every :doc:`Database I/O Action <dbio>`.

``slick.compiler.QueryCompiler``
   Shows a dump of the AST after every phase of the query compiler when compiling a query.

``slick.jdbc.DriverDataSource``
   Shows information about JDBC_ drivers being loaded by :api:`slick.jdbc.DriverDataSource`. Does not
   apply when using a connection pool unless you explicitly use a DriverDataSource as the source for the connection
   pool.

``slick.jdbc.JdbcBackend.statement``
   Shows all SQL statements which are executed.

``slick.jdbc.JdbcBackend.benchmark``
   Shows execution times for SQL statements.

``slick.jdbc.StatementInvoker.result``
   Shows the first rows of the result set when a query is executed. Does not apply to streaming results.
