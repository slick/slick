.. index:: TestKit, testing
.. index::
   pair: driver; custom

Slick TestKit
=============

.. note::
   This chapter is based on the `Slick TestKit Example template`_.
   The prefered way of reading this introduction is in Activator_, where you can
   edit and run the code directly while reading the tutorial.

When you write your own database driver for Slick, you need a way to run all
the standard unit tests on it (in addition to any custom tests you may want to
add) to ensure that it works correctly and does not claim to support any
capabilities which are not actually implemented. For this purpose the Slick
unit tests have been factored out into a separate Slick TestKit project.

To get started, you can clone the `Slick TestKit Example template`_ which
contains a copy of Slick's standard PostgreSQL driver and all the infrastructure
required to build and test it.

Scaffolding
-----------

Its ``build.sbt`` file is straight-forward. Apart from the usual name and
version settings, it adds the dependencies for Slick, the TestKit,
junit-interface, Logback and the PostgreSQL JDBC driver, and it sets some
options for the test runs:

.. parsed-literal::
  libraryDependencies ++= Seq(
    "com.typesafe.slick" %% "slick" % "|release|",
    "com.typesafe.slick" %% "slick-testkit" % "|release|" % "test",
    "com.novocode" % "junit-interface" % "0.10" % "test",
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

.. index:: DriverTest, TestDB

Test Harness
------------

In order to run the TestKit tests, you need to add a class that extends
``DriverTest``, plus an implementation of ``TestDB`` which tells the TestKit
how to connect to a test database, get a list of tables, clean up between
tests, etc.

In the case of the PostgreSQL test harness (in ``src/test/slick/driver/test/MyPostgresTest.scala``)
most of the default implementations can be used out of the box. Only ``localTables`` and
``getLocalSequences`` require custom implementations. We also modify the driver's ``capabilities``
to indicate that our driver does not support the JDBC ``getFunctions`` call::

  @RunWith(classOf[Testkit])
  class MyPostgresTest extends DriverTest(MyPostgresTest.tdb)

  object MyPostgresTest {
    def tdb = new ExternalJdbcTestDB("mypostgres") {
      val driver = MyPostgresDriver
      override def localTables(implicit ec: ExecutionContext): DBIO[Vector[String]] =
        ResultSetAction[(String,String,String, String)](_.conn.getMetaData().getTables("", "public", null, null)).map { ts =>
          ts.filter(_._4.toUpperCase == "TABLE").map(_._3).sorted
        }
      override def getLocalSequences(implicit session: profile.Backend#Session) = {
        val tables = ResultSetInvoker[(String,String,String, String)](_.conn.getMetaData().getTables("", "public", null, null))
        tables.buildColl[List].filter(_._4.toUpperCase == "SEQUENCE").map(_._3).sorted
      }
      override def capabilities = super.capabilities - TestDB.capabilities.jdbcMetaGetFunctions
    }
  }

.. index:: testkit.conf, ExternalTestDB, test-dbs

The name of a configuration prefix, in this case ``mypostgres``, is passed to ``ExternalJdbcTestDB``::

  def tdb =
    new ExternalJdbcTestDB("mypostgres") ...

Database Configuration
----------------------

Since the PostgreSQL test harness is based on ``ExternalJdbcTestDB``, it needs to be configured in
``test-dbs/testkit.conf``::

  mypostgres.enabled = true
  mypostgres.user = myuser
  mypostgres.password = secret

There are several other configuration options that need to be set for an ``ExternalJdbcTestDB``.
These are defined with suitable defaults in ``testkit-reference.conf`` so that ``testkit.conf`` can
be kept very simple in most cases.

Testing
-------

Running ``sbt test`` discovers ``MyPostgresTest`` and runs it with TestKit's
JUnit runner. This in turn causes the database to be set up through the test
harness and all tests which are applicable for the driver (as determined by
the ``capabilities`` setting in the test harness) to be run.
