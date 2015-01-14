.. index:: blocking

Blocking Database Calls
=======================

Slick is designed for being used from an asynchronous, non-blocking framework like Play_ or Akka_. The standard
API for :ref:`executing Actions <executing-actions>` returns either a ``Future`` or a `Reactive Streams`_
``Publisher`` for asynchronous execution. Any kind of blocking calls needed to interface with a blocking API like
JDBC_ are handled internally by Slick. Unless you are *implementing* your own low-level Actions for JDBC, you should
never call blocking APIs.

However, there can be situations when running outside of such an asynchronous framework (e.g. in a standalone code
snippet that is executed as a command-line application) where a blocking API is convenient. Slick provides some
utility methods in :blockingapi:`scala.slick.blocking.Blocking <scala.slick.blocking.Blocking$>` for this purpose,
which are available in a separate module *slick-blocking* that you have to explicitly add to your build:

.. parsed-literal::
  libraryDependencies += "com.typesafe.slick" %% "slick-blocking" % "|release|"

When using a blocking back-end like :api:`scala.slick.jdbc.JdbcBackend`, these methods avoid unnecessary context
switching and message dispatching by running the blocking calls directly in the caller's thread.
