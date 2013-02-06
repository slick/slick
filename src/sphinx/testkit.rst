Slick TestKit
=============

When you write your own database driver for Slick, you need a way to run all
the standard unit tests on it (in addition to any custom tests you may want to
add) to ensure that it works correctly and does not claim to support any
capabilities which are not actually implemented. For this purpose the Slick
unit tests have been factored out into a separate Slick TestKit project.

To get started, you can clone the `Slick TestKit Example`_ project which
contains a (slightly outdated) version of Slick's standard PostgreSQL driver
and all the infrastructure required to build and test it.

Scaffolding
-----------

Its ``build.sbt`` file is straight-forward. Apart from the usual name and
version settings, it adds the dependencies for Slick, the TestKit,
junit-interface, Logback and the PostgreSQL JDBC driver, and it sets some
options for the test runs::

  libraryDependencies ++= Seq(
    "com.typesafe.slick" %% "slick" % "1.0.0",
    "com.typesafe.slick" %% "slick-testkit" % "1.0.0" % "test",
    "com.novocode" % "junit-interface" % "0.10-M1" % "test",
    "ch.qos.logback" % "logback-classic" % "0.9.28" % "test",
    "postgresql" % "postgresql" % "9.1-901.jdbc4" % "test"
  )

  testOptions += Tests.Argument(TestFrameworks.JUnit, "-q", "-v", "-s", "-a")

  parallelExecution in Test := false

  logBuffered := false

There is a copy of Slick's logback configuration in
``src/test/resources/logback-test.xml`` but you can swap out the logging
framework if you prefer a different one.

Driver
------

The actual driver implementation can be found under ``src/main/scala``.

Test Harness
------------

In order to run the TestKit tests, you need to add a class that extends
``DriverTest``, plus an implementation of ``TestDB`` which tells the TestKit
how to connect to a test database, get a list of tables, clean up between
tests, etc.

In the case of the PostgreSQL test harness (in
``src/test/scala/scala/slick/driver/test/MyPostgresTest.scala``) most of the
default implementations can be used out of the box::

  @RunWith(classOf[Testkit])
  class MyPostgresTest extends DriverTest(MyPostgresTest.tdb)

  object MyPostgresTest {
    def tdb(cname: String) = new ExternalTestDB("mypostgres", MyPostgresDriver) {
      override def getLocalTables(implicit session: Session) = {
        val tables = ResultSetInvoker[(String,String,String, String)](_.conn.getMetaData().getTables("", "public", null, null))
        tables.list.filter(_._4.toUpperCase == "TABLE").map(_._3).sorted
      }
      override def getLocalSequences(implicit session: Session) = {
        val tables = ResultSetInvoker[(String,String,String, String)](_.conn.getMetaData().getTables("", "public", null, null))
        tables.list.filter(_._4.toUpperCase == "SEQUENCE").map(_._3).sorted
      }
      override lazy val capabilities = driver.capabilities + TestDB.plainSql
    }
  }

Database Configuration
----------------------

Since the PostgreSQL test harness is based on ``ExternalTestDB``, it needs to
be configured in ``test-dbs/databases.properties``::

  # PostgreSQL quick setup:
  # - Install PostgreSQL server with default options
  # - Change password in mypostgres.password
  # - Set mypostgres.enabled = true
  mypostgres.enabled = false
  mypostgres.url = jdbc:postgresql:[DB]
  mypostgres.user = postgres
  mypostgres.password = secret
  mypostgres.adminDB = postgres
  mypostgres.testDB = slick-test
  mypostgres.create = CREATE TABLESPACE slick_test LOCATION '[DBPATH]'; CREATE DATABASE "[DB]" TEMPLATE = template0 TABLESPACE slick_test
  mypostgres.drop = DROP DATABASE IF EXISTS "[DB]"; DROP TABLESPACE IF EXISTS slick_test
  mypostgres.driver = org.postgresql.Driver

Testing
-------

Running ``sbt test`` discovers ``MyPostgresTest`` and runs it with TestKit's
JUnit runner. This in turn causes the database to be set up through the test
harness and all tests which are applicable for the driver (as determined by
the ``capabilities`` setting in the test harness) to be run.
