Slick
=====

Slick is a modern database query and access library for Scala. It allows you
to work with stored data almost as if you were using Scala collections while
at the same time giving you full control over when a database access happens
and which data is transferred. You can write your database queries in Scala
instead of SQL, thus profiting from the static checking, compile-time safety
and compositionality of Scala. Slick features an extensible query compiler
which can generate code for different backends.

The following database systems are directly supported for type-safe queries:

- Derby/JavaDB
- H2
- HSQLDB/HyperSQL
- Microsoft Access
- Microsoft SQL Server
- MySQL
- PostgreSQL
- SQLite

Support for DB2 and Oracle is scheduled to be available for production use by
[Typesafe subscribers](http://www.typesafe.com/products/typesafe-subscription)
(free for evaluation and development) along with release 1.0 of Slick.

Accessing other database systems is possible, with a reduced feature set.

The [manual and scaladocs](http://slick.typesafe.com/docs/) for Slick can be
found on the [Slick web site](http://slick.typesafe.com/).
There is some older documentation (which may still apply to some extent to
Slick) in the [ScalaQuery Wiki](https://github.com/szeiger/scala-query/wiki).

Licensing conditions (BSD-style) can be found in LICENSE.txt.
