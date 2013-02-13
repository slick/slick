Introduction
############

What is Slick?
--------------

Slick is a modern database query and access library for Scala. It allows you
to work with stored data almost as if you were using Scala collections while
at the same time giving you full control over when a database access happens
and which data is transferred. You can write your database queries in Scala
instead of SQL, thus profiting from the static checking, compile-time safety
and compositionality of Scala. Slick features an extensible query compiler
which can generate code for different backends.

It includes the following features:

* Lifted query embedding (fully featured, composable, uses lifted types, evolved from ScalaQuery)
* Direct query embedding (experimental, limited features, uses ordinary Scala types, based on macros)
* Simple execution of raw SQL queries
* Session management based on JDBC Connections

Slick requires Scala 2.10. ScalaQuery_, the predecessor of Slick, is available
for Scala 2.9.

.. _supported-dbs:

Supported database systems
--------------------------

The following database systems are directly supported for type-safe queries
(with lifted and direct embedding):

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

Accessing other databases is possible, with a reduced feature set.

Quick Overview
--------------

Accessing databases using Slick's lifted embedding requires the following steps.

#. Add the Slick jar and its dependencies to your project
#. Pick a driver for a particular db and create a session (or simply pick threadLocalSession)

   .. includecode:: code/GettingStartedOverview.scala#quick-imports

#. Describe your Database schema

   .. includecode:: code/GettingStartedOverview.scala#quick-schema

#. Write queries using for-comprehensions or map/flatMap wrapped in a session scope

   .. includecode:: code/GettingStartedOverview.scala#quick-query

The :doc:`next chapter <gettingstarted>` explains these steps and further aspects in more detail.
