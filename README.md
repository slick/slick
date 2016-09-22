Slick
=====


LifeOnAir

We've merged the pending PR #966 by @tminglei from //github.com/tminglei/slick.git, in order to fix an SQL code generation error in insertOrUpdate().

Version name changed from 3.2.0-M1 to 3.2.0-M1-lifeonair. Also, added artifactory publihing capability.


Original README:




[![Build Status](https://travis-ci.org/slick/slick.png?branch=3.1)](https://travis-ci.org/slick/slick) [![Stories in Ready](https://badge.waffle.io/slick/slick.png?label=1%20-%20Ready)](https://waffle.io/slick/slick) [![Gitter](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/slick/slick?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge)

Slick is a modern database query and access library for Scala. It allows you
to work with stored data almost as if you were using Scala collections while
at the same time giving you full control over when a database access happens
and which data is transferred. You can write your database queries in Scala
instead of SQL, thus profiting from the static checking, compile-time safety
and compositionality of Scala. Slick features an extensible query compiler
which can generate code for different backends.

The following database systems are directly supported for type-safe queries.
These are the databases and driver versions that have explicit automated tests.

|Database|JDBC Driver|Build status|
|--------|-----------|-----------:|
|SQLServer 2008, 2012, 2014|[jtds:1.2.8](http://sourceforge.net/projects/jtds/files/jtds/) and [msjdbc:4.2](https://www.microsoft.com/en-gb/download/details.aspx?id=11774)|[![Build status](https://ci.appveyor.com/api/projects/status/mdrfd7o7067c5vcm?svg=true&branch=master)](https://ci.appveyor.com/project/slick/slick)|
|Oracle 11g|[ojdbc7:12.1.0.2](http://www.oracle.com/technetwork/database/features/jdbc/index-091264.html)|[![Build Status](https://travis-ci.org/slick/slick.svg?branch=master)](https://travis-ci.org/slick/slick)|
|DB2 10.5|[db2jcc4:4.19.20](http://www-01.ibm.com/support/docview.wss?uid=swg21363866)|[![Build Status](https://travis-ci.org/slick/slick.svg?branch=master)](https://travis-ci.org/slick/slick)|
|MySQL|mysql-connector-java:5.1.23|[![Build Status](https://travis-ci.org/slick/slick.svg?branch=master)](https://travis-ci.org/slick/slick)|
|PostgreSQL|postgresql:9.1-901.jdbc4|[![Build Status](https://travis-ci.org/slick/slick.svg?branch=master)](https://travis-ci.org/slick/slick)|
|SQLite|sqlite-jdbc:3.8.7|[![Build Status](https://travis-ci.org/slick/slick.svg?branch=master)](https://travis-ci.org/slick/slick)|
|Derby/JavaDB|derby:10.9.1.0|[![Build Status](https://travis-ci.org/slick/slick.svg?branch=master)](https://travis-ci.org/slick/slick)|
|HSQLDB/HyperSQL|hsqldb:2.2.8|[![Build Status](https://travis-ci.org/slick/slick.svg?branch=master)](https://travis-ci.org/slick/slick)|
|H2|com.h2database.h2:1.4.187|[![Build Status](https://travis-ci.org/slick/slick.svg?branch=master)](https://travis-ci.org/slick/slick)|


Accessing other database systems is possible, with a reduced feature set.

The [manual and scaladocs](http://slick.typesafe.com/docs/) for Slick can be
found on the [Slick web site](http://slick.typesafe.com/).

Licensing conditions (BSD-style) can be found in LICENSE.txt.
