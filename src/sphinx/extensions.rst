Slick Extensions
================

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
