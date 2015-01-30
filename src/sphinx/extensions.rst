Extensions
================

Typesafe / EPFL
----------------

Play-Slick
__________
The official Play plugin for Slick

`Github project <https://github.com/playframework/play-slick>`_

`Wiki <https://github.com/playframework/play-slick/wiki>`_


Typesafe's commercial drivers for Oracle, DB2 and Microsoft SQL Server.
__________________________________________________________________________

Slick drivers for Oracle (``com.typesafe.slick.driver.oracle.OracleDriver``),
IBM DB2 (``com.typesafe.slick.driver.db2.DB2Driver``) and Microsoft SQL Server
(``com.typesafe.slick.driver.ms.SQLServerDriver``) are available in
*slick-extensions*, a closed-source package with commercial support
provided by Typesafe, Inc. It is made available under the terms and conditions
of the `Typesafe Subscription Agreement`_ (PDF).

If you are using sbt_, you can add *slick-extensions* and the Typesafe
repository (which contains the required artifacts) to your build definition
like this:

.. parsed-literal::
  libraryDependencies += "com.typesafe.slick" %% "slick-extensions" % "|release|"

  resolvers += "Typesafe Releases" at "http://repo.typesafe.com/typesafe/maven-releases/"

Third-party
-----------

This is a list of third-party Slick extension projects we know of. We cannot guarantee for the quality or that they represent our view of things. Please add more projects to the list using a github pull request, if you think others can benefit from them. 


Slick Joda Date mapper
_________________________
Enables you to use joda-time with Slick. You can persist DateTime, Instant, LocalDateTime, LocalDate, LocalTime, DateTimeZone with Slick.

https://github.com/tototoshi/slick-joda-mapper


Slick Postgres extensions
_________________________

Slick extensions for PostgreSQL, to support a series of pg data types and related operators/functions.

https://github.com/tminglei/slick-pg


Generic DAO for Slick projects
__________________________________________________

https://github.com/rcavalcanti/slick-dao


Slick macros
_____________
https://github.com/ebiznext/slick-macros

