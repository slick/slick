Slick - Scala Language Integrated Connection Kit
################################################

Project Slick (Scala Language Integrated Connection Kit) aims at the
development of a complete, modern, convenient, efficient and safe data query
and access framework for the Scala programming language that operates on the
Java platform. Ordinary Scala code can be used to query different database
back-ends. Queries can be processed and validated at compile time, even in
user-provided back-ends.

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

.. toctree::
   :maxdepth: 4
   
   gettingstarted
   direct-embedding
   sql
