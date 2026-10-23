# Building and testing Slick

Run sbt from the repository root. The sbt 2 launcher joins all its arguments into one command line, so give
several commands as one quoted argument separated by `;`.

```bash
sbt testAll               # what CI runs: all test suites, then the scaladoc builds
sbt testkit/test          # incremental: skips suites that passed before and are unaffected by your changes
sbt testkit/testFull      # every testkit suite
sbt testkit/DocTest/test  # the examples in the manual
sbt "++3.x testkit/test"  # under the Scala 3 version in `crossScalaVersions` (likewise ++2.12.x, ++2.13.x)
```

## Working in a linked git worktree

In a worktree created with `git worktree add`, the build fails to load with
`org.eclipse.jgit.errors.NoWorkTreeException: Bare Repository has neither a working tree, nor an index`: sbt-git
reads the repository through JGit, which fails there. Set `SBT_GIT_USE_CONSOLE_FOR_RO_GIT=1` to make sbt-git
run the `git` command instead.

## Selecting testkit tests

Most of Slick's tests are testkit test classes in `com.typesafe.slick.testkit.tests` (`slick-testkit/src/main`).
sbt does not run them as suites of their own. Each *profile test* in `slick.test.profile` (`slick-testkit/src/test`),
such as `H2MemTest` or `PostgresTest`, runs all of them against one database, so a test's name is the test class, the
method and the database's config name: `com.typesafe.slick.testkit.tests.JoinTest.testJoin[h2mem]`. A `testOnly`
pattern selects profile tests, never testkit test classes (`testkit/testOnly *JoinTest*` runs nothing); to select
tests within the profile tests, pass arguments to junit-interface after `--`:

```bash
# One test
sbt "testkit/testOnly slick.test.profile.H2MemTest -- com.typesafe.slick.testkit.tests.JoinTest.testJoin[h2mem]"
# One test class
sbt "testkit/testOnly slick.test.profile.H2MemTest -- *.JoinTest.*"
# One test class on every database (service databases only if enabled, see below)
sbt "testkit/testOnly slick.test.profile.* -- *.JoinTest.*"
# Several patterns: a test runs if it matches any of them
sbt "testkit/testOnly slick.test.profile.H2MemTest -- *.JoinTest.testJoin[*] *.TransactionTest.*"
```

Each argument that does not start with `-` or `+` is a glob matched against the whole test name, with `*`
matching any characters. Alternatively, `--tests=<regex>` matches a regular expression against the method name and
database name only (`testJoin[h2mem]`), so `--tests=testJoin.*` runs the `testJoin` method of every test class that
has one; it reports every test it skips as ignored. junit-interface silently ignores options it does not know, such as
`-z`: in `-z <name>`, `<name>` is an ordinary glob.

The other suites in `slick-testkit/src/test` are ordinary test classes, which `testOnly` selects by class name,
e.g. `sbt "testkit/testOnly slick.test.codegen.*"`. Before running the codegen tests, delete the generated
sources so they are regenerated: `rm -rf target/out/jvm/scala-*/slick-testkit/src_managed`.

## Setting system properties for tests

Tests run in forked JVMs, which do not receive `-D` options given to sbt itself. junit-interface sets
`-Dkey=value` arguments given after `--` in the test JVM. To print the SQL statements that tests execute:

```bash
sbt "testkit/testOnly slick.test.profile.H2MemTest -- +q -Dlog.jdbc.statement=debug *.JoinTest.*"
```

`+q` shows the output of passing tests, which the build's default `-q` hides. The other logger properties are
listed in [common-test-resources/logback.xml](common-test-resources/logback.xml).

## Testing against PostgreSQL, MySQL, Oracle, DB2 and SQL Server

The embedded databases (H2, Derby, HSQLDB, SQLite) are always tested. The others are *service databases*: their
profile tests report every test as ignored, and pass, unless the database is enabled in the testkit configuration.
[test-dbs/testkit-databases.conf](test-dbs/testkit-databases.conf) holds connection settings matching the services
in [docker-compose.yml](docker-compose.yml), with every database disabled. Each database is enabled by its
`<name>.enabled = true` setting, where `<name>` is `postgres`, `mysql`, `oracle`, `db2` or `sqlserver-sqljdbc`.

The test JVM reads the configuration file named by the system property `slick.testkit-config`, or else by the
environment variable `SLICK_TESTKIT_CONFIG` (default: `test-dbs/testkit.conf`), with system properties overriding
its settings. For a `testOnly` run, pass both as arguments after `--`:

```bash
docker compose up -d postgres
sbt "testkit/testOnly slick.test.profile.PostgresTest -- -Dslick.testkit-config=test-dbs/testkit-databases.conf -Dpostgres.enabled=true"
```

`testFull` and `testAll` take no arguments, so put the settings in a file and name it in `SLICK_TESTKIT_CONFIG`.
Files matching `test-dbs/*.local.conf` are ignored by git:

```hocon
# test-dbs/postgres.local.conf
include "testkit-databases.conf"
postgres.enabled = true
```

```bash
SLICK_TESTKIT_CONFIG=test-dbs/postgres.local.conf sbt --server testkit/testFull
```

Use `testFull`: `test` would skip the profile tests that passed before, including those that passed with every test
ignored, because the testkit configuration is not among its inputs. Test JVMs inherit the environment of the sbt
server, and by default the `sbt` command hands its commands to a background server, which keeps the environment it
was started with. `--server` runs sbt in the foreground with the current environment. CI runs `testAll` with
[test-dbs/testkit.github-actions.conf](test-dbs/testkit.github-actions.conf), which enables every service database.

Testing a service database drops and recreates its test database (`slick_test` on PostgreSQL) and the tables in it,
so two runs against the same server interfere with each other. To test databases installed without Docker, see
[test-dbs/README.md](test-dbs/README.md).
