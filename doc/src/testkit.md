Slick TestKit {index="profile,custom; custom,profile; TestKit; testing"}
=============

When you write your own database profile for Slick, you need a way to run all
the standard unit tests on it (in addition to any custom tests you may want to
add) to ensure that it works correctly and does not claim to support any
capabilities which are not actually implemented. For this purpose the Slick
unit tests have been factored out into a separate *Slick TestKit* project.

To get started, you can [clone](samplerepo:slick-testkit-example) or [download](samplezip:slick-testkit-example)
the **TestKit Example** project which contains a copy of Slick's standard PostgreSQL profile and all the infrastructure
required to build and test it.

Scaffolding
-----------

Its `build.sbt` file is straight-forward. It adds the dependencies for Slick, TestKit, junit-interface, Logback and
the PostgreSQL JDBC driver, and it sets some options for the test runs:

```scala src=../../samples/slick-testkit-example/build.sbt
```

There is a copy of Slick's logback configuration in `src/test/resources/logback-test.xml` but you can swap out the
logging framework if you prefer a different one.

Profile
-------

The actual profile implementation can be found under `src/main/scala`:

```scala src=../../samples/slick-testkit-example/src/main/scala/MyPostgresProfile.scala#outline
```

Test Harness {index="ProfileTest; TestDB; testkit.conf; ExternalTestDB; test-dbs"}
------------

In order to run the TestKit tests, you need to add a class that extends `ProfileTest`, plus an implementation of
`TestDB` which tells TestKit how to connect to a test database, get a list of tables, clean up between
tests, etc.

In the case of the PostgreSQL test harness (in `src/test/scala/MyPostgresTest.scala`)
most of the default implementations can be used out of the box. Only `localTables` and
`getLocalSequences` require custom implementations. We also modify the profile's `capabilities`
to indicate that our profile does not support the JDBC `getFunctions` call:

```scala src=../../samples/slick-testkit-example/src/test/scala/MyPostgresTest.scala#outline
```

The name of a configuration prefix, in this case `mypostgres`, is passed to `ExternalJdbcTestDB`:

```scala src=../../samples/slick-testkit-example/src/test/scala/MyPostgresTest.scala#tdb
```

Database Configuration
----------------------

Since the PostgreSQL test harness is based on `ExternalJdbcTestDB`, it needs to be configured in
`test-dbs/testkit.conf`:

```yaml src=../../samples/slick-testkit-example/test-dbs/testkit.conf
```

There are several other configuration options that need to be set for an `ExternalJdbcTestDB`.
These are defined with suitable defaults in `testkit-reference.conf` so that `testkit.conf` can
be kept very simple in most cases:

```yaml src=../../samples/slick-testkit-example/src/test/resources/testkit-reference.conf
```

Testing
-------

Running `sbt test` discovers `MyPostgresTest` and runs it with TestKit's
JUnit runner. This in turn causes the database to be set up through the test
harness and all tests which are applicable for the profile (as determined by
the `capabilities` setting in the test harness) to be run.
