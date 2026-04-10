Configuration
=============

In addition to configuring your  @ref:[database connections](database.md) in `application.conf` you can also set some
global options in this file or through system properties (see the @extref[Typesafe Config](typesafe-config:) documentation for details).
These are the available options and their default values as defined in Slick's own `reference.conf`:

@@snip [reference.conf](../../../slick/src/main/resources/reference.conf)

Logging
-------

Slick uses @extref[SLF4J](slf4j:) for logging. How to configure loggers and appenders depends on the actual logging framework that you
use in your application. The following @extref[Logback](logback:) configuration is used internally for testing and debugging of Slick. It
contains a list of all loggers that are used by Slick for debug output:

@@snip [logback.xml](../../../common-test-resources/logback.xml)

@@@ note
Messages on WARNING and ERROR levels may also be emitted by loggers that are not explicitly mentioned in this
configuration.
@@@

You should generally enable logging for the root package `slick` at level `INFO` (currently unused)
or `WARNING`. Debug logging should only be enabled selectively for individual loggers, otherwise you will get a huge
amount of log output.

The following loggers are particularly interesting:

- `slick.basic.BasicBackend.action`:
   Shows the execution of every  @ref:[Database I/O Action](dbio.md).

- `slick.compiler.QueryCompiler`:
   Shows a dump of the AST after every phase of the query compiler when compiling a query.

- `slick.jdbc.DriverDataSource`:
   Shows information about @extref[JDBC](jdbc:) drivers being loaded by @scaladoc[DriverDataSource](slick.jdbc.DriverDataSource). Does not
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

Monitoring
----------

Slick does not expose its own JMX bean in Slick 4. Observability is provided at two levels:

1. **Connection pool (HikariCP)**: When a @ref:[Database](database.md) object has the `registerMbeans` option
   enabled, the @extref[HikariCP](hikaricp:) pool implementation registers @extref[JMX](jmx:) management beans.
   See [HikariCP Monitoring] in the HikariCP documentation for details.

2. **Effect runtime**: Cats Effect 3 runtimes (including the default `IORuntime`) expose metrics through
   standard JVM tooling. If you are using a framework like http4s, refer to its documentation for
   integrating metrics (e.g., via Micrometer or Prometheus) with the CE3 runtime.

In addition to pool/runtime observability, Slick 4's execution limits are configured directly on
the database config section:

- `maxConnections`: maximum concurrent JDBC connections.
- `maxInflightActions` (default `2 * maxConnections`): maximum concurrently running DBIO chains.
- `queueSize` (default `1000`): maximum callers waiting for in-flight admission.

When `queueSize` is exhausted, Slick rejects immediately with
`SlickException("DBIOAction queue full")`.

Slick also exposes runtime concurrency gauges on `Database[F]`:

- `availableConnectionSlots`: free connection slots in the connection arbiter.
- `pendingConnectionSlots`: callers currently waiting for a connection slot.
- `availableAdmissionQueueSlots`: free waiting-room slots before queue-full rejection.
- `availableInflightSlots`: free in-flight DBIO action slots.

These values are useful to distinguish pressure points: admission saturation, inflight saturation,
or connection-slot contention.

The management bean names registered by HikariCP are qualified with the `poolName` from the database
configuration, or the config path if `poolName` has not been set explicitly.

Example: Including the following configuration options in the database configuration

```yaml
connectionPool = "HikariCP"
registerMbeans = true
poolName = "myDb"
```

results in these two HikariCP management beans being registered:

- `com.zaxxer.hikari:type=PoolConfig (myDb)`
- `com.zaxxer.hikari:type=Pool (myDb)`
