Slick
=====

[![Build Status](https://travis-ci.org/slick/slick.png?branch=3.1)](https://travis-ci.org/slick/slick) [![Stories in Ready](https://badge.waffle.io/slick/slick.png?label=1%20-%20Ready)](https://waffle.io/slick/slick) [![Gitter](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/slick/slick?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge)

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
- MySQL
- PostgreSQL
- SQLite
- Oracle 11g
- IBM DB2 LUW 10
- Microsoft SQL Server 2008

Accessing other database systems is possible, with a reduced feature set.

The [manual and scaladocs](http://slick.typesafe.com/docs/) for Slick can be
found on the [Slick web site](http://slick.typesafe.com/).

Licensing conditions (BSD-style) can be found in LICENSE.txt.
