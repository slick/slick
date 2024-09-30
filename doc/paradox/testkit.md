Slick TestKit
=============

When you write your own database profile for Slick, you need a way to run all
the standard unit tests on it (in addition to any custom tests you may want to
add) to ensure that it works correctly and does not claim to support any
capabilities which are not actually implemented. For this purpose the Slick
unit tests have been factored out into a separate *Slick TestKit* project.

To get started, you can download @extref[the **TestKit Example** project](samplerepo:slick-testkit-example) which
contains a copy of Slick's standard PostgreSQL profile and all the infrastructure required to build and test it.

Scaffolding
-----------

Its `build.sbt` file is straight-forward. It adds the dependencies for Slick, TestKit, junit-interface, Logback and
the PostgreSQL JDBC driver, and it sets some options for the test runs:

@@snip [build.sbt](samples/slick-testkit-example/build.sbt)

There is a copy of Slick's logback configuration in `src/test/resources/logback-test.xml` but you can swap out the
logging framework if you prefer a different one.

Profile
-------

The actual profile implementation can be found under `src/main/scala`:

@@snip [MyPostgresProfile.scala](samples/slick-testkit-example/src/main/scala/slick/examples/testkit/MyPostgresProfile.scala) { #outline }

Test Harness
------------

In order to run the TestKit tests, you need to add a class that extends `ProfileTest`, plus an implementation of
`TestDB` which tells TestKit how to connect to a test database, get a list of tables, clean up between
tests, etc.

In the case of the PostgreSQL test harness (in `src/test/scala/MyPostgresTest.scala`)
most of the default implementations can be used out of the box. Only `localTables` and
`getLocalSequences` require custom implementations. We also modify the profile's `capabilities`
to indicate that our profile does not support the JDBC `getFunctions` call:

@@snip [MyPostgresTest.scala](samples/slick-testkit-example/src/test/scala/slick/examples/testkit/MyPostgresTest.scala) { #outline }

The name of a configuration prefix, in this case `mypostgres`, is passed to `ExternalJdbcTestDB`:

@@snip [MyPostgresTest.scala](samples/slick-testkit-example/src/test/scala/slick/examples/testkit/MyPostgresTest.scala) { #tdb }

Database Configuration
----------------------

Since the PostgreSQL test harness is based on `ExternalJdbcTestDB`, it needs to be configured in
`test-dbs/testkit.conf`:

@@snip [testkit.conf](samples/slick-testkit-example/test-dbs/testkit.conf)

There are several other configuration options that need to be set for an `ExternalJdbcTestDB`.
These are defined with suitable defaults in `testkit-reference.conf` so that `testkit.conf` can
be kept very simple in most cases:

@@snip [testkit-reference.conf](samples/slick-testkit-example/src/test/resources/testkit-reference.conf)

Testing
-------

Running `sbt test` discovers `MyPostgresTest` and runs it with TestKit's
JUnit runner. This in turn causes the database to be set up through the test
harness and all tests which are applicable for the profile (as determined by
the `capabilities` setting in the test harness) to be run.
