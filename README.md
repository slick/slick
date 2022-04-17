Slick
=====

[![Maven](https://img.shields.io/maven-central/v/com.typesafe.slick/slick_2.13.svg)](http://mvnrepository.com/artifact/com.typesafe.slick/slick_2.13) [![Gitter](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/slick/slick?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge)

Slick is a modern database query and access library for Scala. It allows you
to work with stored data almost as if you were using Scala collections while
at the same time giving you full control over when a database access happens
and which data is transferred. You can write your database queries in Scala
instead of SQL, thus profiting from the static checking, compile-time safety
and compositionality of Scala. Slick features an extensible query compiler
which can generate code for different backends.

The following database systems are directly supported for type-safe queries.
These are the databases and driver versions that have explicit automated tests.

|Database|JDBC Driver|
|--------|-----------|
|SQLServer 2008, 2012, 2014, 2017|[jtds:1.3.1](http://sourceforge.net/projects/jtds/files/jtds/) and [msjdbc:7.2.2](https://docs.microsoft.com/en-us/sql/connect/jdbc/download-microsoft-jdbc-driver-for-sql-server?view=sql-server-2017)|
|Oracle 11g|[ojdbc7:12.1.0.2](http://www.oracle.com/technetwork/database/features/jdbc/index-091264.html)|
|DB2 10.5|[db2jcc4:4.19.20](http://www-01.ibm.com/support/docview.wss?uid=swg21363866)|
|MySQL|mysql-connector-java:8.0.16|
|PostgreSQL|postgresql:42.2.5|
|SQLite|sqlite-jdbc:3.27.2.1|
|Derby/JavaDB|derby:10.14.2.0|
|HSQLDB/HyperSQL|hsqldb:2.4.1|
|H2|com.h2database.h2:1.4.199|

Accessing other database systems is possible, with a reduced feature set.

The [manual and scaladocs](https://scala-slick.org/docs/) for Slick can be
found on the [Slick web site](https://scala-slick.org).

## Maintenance status

Slick is community-maintained by a loose assortment of volunteers.
Please help if you can. We talk on [Gitter](https://gitter.im/slick/slick).

Lightbend staff (such as @SethTisue) may be able to assist with
administrative issues.
