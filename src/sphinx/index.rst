Slick - Scala Language Integrated Connection Kit
################################################

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

.. _supported-dbs:

Supported database systems
--------------------------

The following database systems are directly supported for type-safe queries
(with lifted and direct embedding):

* Derby/JavaDB
* H2
* HSQLDB/HyperSQL
* Microsoft Access
* Microsoft SQL Server
* MySQL
* PostgreSQL
* SQLite

Accessing other databases is possible, with a reduced feature set.

Table of Contents
-----------------

.. toctree::
   :maxdepth: 4
   
   gettingstarted
   lifted-embedding
   direct-embedding
   sql
