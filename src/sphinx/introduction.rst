:tocdepth: 2

Introduction
############

What is Slick?
--------------

Slick is `Typesafe <http://www.typesafe.com>`_'s modern database query and access library for Scala. It allows you
to work with stored data almost as if you were using Scala collections while
at the same time giving you full control over when a database access happens
and which data is transferred. You can also use SQL directly.

.. includecode:: code/GettingStartedOverview.scala#what-is-slick-micro-example

When using Scala instead of SQL for your queries you profit from the compile-time safety
and compositionality. Slick can generate queries for different backends including
your own, using its extensible query compiler.

Why Slick? / Features
----------------------------

Slick offers a unique combination of features:

Easy
^^^^^
* Access stored data just like Scala collections
* Unified session management based on JDBC Connections
* Supports SQL if you need it
* Simple setup

Concise
^^^^^^^^
* Scala syntax
* Fetch results without pain (no ResultSet.getX)

Scales naturally
^^^^^^^^^^^^^^^^
* Stateless (like the web)
* Explicit control of execution time and transferred data

Safe
^^^^^^^^
* No SQL-injections
* Compile-time safety (types, names, no typos, etc.)
* Type-safe support for custom database functions

Composable
^^^^^^^^^^^^^^^^
* It‘s Scala code: abstract and re-use with ease


Slick requires Scala 2.10. (For Scala 2.9 please use ScalaQuery_, the predecessor of Slick).

.. _supported-dbs:


Supported database systems
--------------------------

* DB2 (via :doc:`slick-extensions <extensions>`)
* Derby/JavaDB
* H2
* HSQLDB/HyperSQL
* Microsoft Access
* Microsoft SQL Server
* MySQL
* Oracle (via :doc:`slick-extensions <extensions>`)
* PostgreSQL
* SQLite

Other SQL databases can be accessed right away with a reduced feature set.
Writing a fully featured plugin for your own SQL-based backend can be achieved
with a reasonable amount of work. Support for other backends (like NoSQL) is
under development but not yet available.

Quick Overview
--------------

Accessing databases using Slick's lifted embedding requires the following steps.

#. Add the Slick jar and its dependencies to your project
#. Pick a driver for a particular db

   .. includecode:: code/GettingStartedOverview.scala#quick-imports

#. Describe your Database schema

   .. includecode:: code/GettingStartedOverview.scala#quick-schema

#. Write queries using for-comprehensions or map/flatMap wrapped in a session scope

   .. includecode:: code/GettingStartedOverview.scala#quick-query

The :doc:`next chapter <gettingstarted>` explains these steps and further aspects in more detail.

License
-------
Slick is released under a BSD-Style free and open source software :slick:`license <LICENSE.txt>`.
