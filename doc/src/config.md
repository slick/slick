Configuration {index="configuration; options"}
=============

In addition to configuring your [database connections](database.md) in `application.conf` you can also set some
global options in this file or through system properties (see the [Typesafe Config] documentation for details).
These are the available options and their default values as defined in Slick's own `reference.conf`:

```yaml src=../../slick/src/main/resources/reference.conf
```

Logging {index="logging; slf4j"}
-------

Slick uses [SLF4J] for logging. How to configure loggers and appenders depends on the actual logging framework that you
use in your application. The following [Logback] configuration is used internally for testing and debugging of Slick. It
contains a list of all loggers that are used by Slick for debug output:

```xml src=../../common-test-resources/logback.xml
```

> {.note}
> Messages on WARNING and ERROR levels may also be emitted by loggers that are not explicitly mentioned in this
> configuration.

You should generally enable logging for the root package `slick` at level `INFO` (currently unused)
or `WARNING`. Debug logging should only be enabled selectively for individual loggers, otherwise you will get a huge
amount of log output.

The following loggers are particularly interesting:

- `slick.basic.BasicBackend.action`:
   Shows the execution of every [Database I/O Action](dbio.md).

- `slick.compiler.QueryCompiler`:
   Shows a dump of the AST after every phase of the query compiler when compiling a query.

- `slick.jdbc.DriverDataSource`:
   Shows information about [JDBC] drivers being loaded by <api:slick.jdbc.DriverDataSource>. Does not
   apply when using a connection pool unless you explicitly use a DriverDataSource as the source for the connection
   pool.

- `slick.jdbc.JdbcBackend.statement`:
   Shows all SQL statements which are executed.

- `slick.jdbc.JdbcBackend.parameter`:
   Shows bind variable contents (for supported types) of all SQL statements which are executed.

- `slick.jdbc.JdbcBackend.benchmark`:
   Shows execution times for SQL statements.

- `slick.jdbc.StatementInvoker.result`:
   Shows the first rows of the result set when a query is executed. Does not apply to streaming results.

Monitoring {index="monitoring"}
----------

When a [Database](database.md) object has the `registerMbeans` option enabled (see
[Database.forConfig](api:slick.jdbc.JdbcBackend$DatabaseFactoryDef@forConfig(String,Config,Driver,ClassLoader):Database),
Slick registers a [JMX] management bean of type <api:slick.util.AsyncExecutorMXBean> that provides information about the
current workload of the database I/O thread pool and the task queue.

Connection pool implementations may also honor this option and register additional management beans. In particular,
the default [HikariCP] pool implementation does this. See [HikariCP Monitoring] in the HikariCP documentation for
details.

The management bean names are qualified with the `poolName` from the database configuration, or the config path
if the `poolName` has not been set explicitly.

Example: Including the following configuration options in the database configuration

```yaml
connectionPool = "HikariCP"
registerMbeans = true
poolName = "myDb"
```

results in these three management beans being registered:

- `slick:type=AsyncExecutor,name=myDb`
- `com.zaxxer.hikari:type=PoolConfig (myDb)`
- `com.zaxxer.hikari:type=Pool (myDb)`
