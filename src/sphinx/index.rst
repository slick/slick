SLICK - Scala Language Integrated Connection Kit
################################################

Project SLICK (Scala Language Integrated Connection Kit) aims at the
development of a complete, modern, convenient, efficient and safe data query
and access framework for the Scala programming language that operates on the
Java platform. Ordinary Scala code can be used to query different database
back-ends. Queries can be processed and validated at compile time, even in
user-provided back-ends.

It includes the following features:

* Query language with type-safe queries
* Writing database queries with plain Scala code
* Simple execution of raw SQL queries
* Session management based on JDBC Connections

The following database systems are directly supported for type-safe queries
(using the query language or Scala code lifted to DB queries):

* Derby/JavaDB
* H2
* HSQLDB/HyperSQL
* Microsoft Access
* Microsoft SQL Server
* MySQL
* PostgreSQL
* SQLite

Accessing other database systems is possible, with a reduced feature set.

.. toctree::
   :maxdepth: 4
   
   gettingstarted.rst
