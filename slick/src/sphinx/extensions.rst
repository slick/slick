.. index:: extensions, Oracle, DB2, SQL Server

Slick Extensions
================

Slick Extensions, a closed-source package with commercial support
provided by Typesafe, Inc contains Slick profiles for:

* Oracle (``com.typesafe.slick.jdbc.OracleProfile``)
* IBM DB2 (``com.typesafe.slick.jdbc.DB2Profile``)
* Microsoft SQL Server (``com.typesafe.slick.jdbc.SQLServerProfile``)

.. note::
   You may use it for development and testing purposes under the terms and conditions
   of the `Typesafe Subscription Agreement`_ (PDF). Production use requires a
   `Typesafe Subscription`_.

If you are using sbt_, you can add *slick-extensions* and the Typesafe
repository (which contains the required artifacts) to your build definition
like this:

.. parsed-literal::
  libraryDependencies += "com.typesafe.slick" %% "slick-extensions" % "|slick_extensions_version|"

  resolvers += "Typesafe Releases" at "http://repo.typesafe.com/typesafe/maven-releases/"
