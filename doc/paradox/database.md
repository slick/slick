Database
========

Working with a database in Slick involves two steps:

1. Build a `DatabaseConfig[P]` — this describes which database profile to use and how to
   connect. Several construction methods are available depending on how your connection
   details are provided.
2. Open a `Database` value from that config using one of Slick's three API facades,
   which adapt Slick's execution model to your chosen effect system:
   - `slick.cats.Database` (Cats Effect + FS2)
   - `slick.future.Database` (Scala Future + Reactive Streams)
   - `slick.zio.Database` (ZIO + ZStream)

Building a DatabaseConfig
--------------------------

### Using Typesafe Config

The easiest way to configure database connections is through @extref[Typesafe Config](typesafe-config:) in your
`application.conf`, which is also used by @extref[Play](play:) and @extref[Akka](akka:) for their configuration.

Such a configuration can be loaded with `DatabaseConfig.forProfileConfig` (see the
@scaladoc[API documentation](slick.basic.DatabaseConfig$#forProfileConfig[P%3C:slick.basic.BasicProfile](profile:P,path:String,config:com.typesafe.config.Config,classLoader:ClassLoader):slick.basic.BasicDatabaseConfig[P])
of this method for details on the configuration parameters). It returns a `DatabaseConfig` value
that you can open with any of the three facades.

@@snip [Connection.scala](../code/Connection.scala) { #forConfig }

#### PostgreSQL
@@snip [application.conf](../code/application.conf) { #postgres }

The `dataSourceClass` is a `javax.sql.DataSource` class provided by the JDBC driver; a Slick profile is not a
`DataSource`. When specifying a dataSourceClass you will need to bring in the sbt dependency for that class. The following is an example for the `org.postgresql.ds.PGSimpleDataSource` data source class:

@@@ vars
```scala
libraryDependencies ++= Seq(
  "com.typesafe.slick" %% "slick" % "$project.version$",
  "org.slf4j" % "slf4j-nop" % "2.0.20",
  "com.typesafe.slick" %% "slick-hikaricp" % "$project.version$",
  "org.postgresql" % "postgresql" % "42.7.14" //org.postgresql.ds.PGSimpleDataSource dependency
)
```
@@@

Note, some examples on the internet point to the 9.4.X versions of the
PostgreSQL JDBC drivers. Those examples are incorrect and many years out of
date. The 42.X development line has been the main line of development since
February 2017. As noted on the Postgres site, "Unless you have unusual
requirements (running old applications or JVMs), this is the driver you should
be using." See https://jdbc.postgresql.org/download.html for more information
about when you may want to use the 9.4 line. Old drivers fail with "The authentication type 10 is not supported"
because they do not support the `scram-sha-256` password authentication that PostgreSQL uses by default.

#### MySQL

Very simple example without a connection pool and using the driver directly:

@@snip [application.conf](../code/application.conf) { #mysql }

To use the MySQL driver, the following library dependency needs to be configured via SBT:

```scala
libraryDependencies += "com.mysql" % "mysql-connector-j" % "8.0.33"
```

#### SQLite

SQLite has no dedicated date and time storage classes. The sqlite-jdbc driver stores `java.sql.Date`,
`java.sql.Time` and `java.sql.Timestamp` values as INTEGER numbers of **milliseconds** since the Unix epoch
(1970-01-01 00:00:00 UTC). Slick uses the same representation for its SQL literals and maps `LocalDate`,
`LocalDateTime` and `Instant` through these types, so they are stored in the same way. `LocalDate` and
`LocalDateTime` values are converted using the JVM's default time zone, so for example
`LocalDate.of(2020, 1, 2)` is stored as midnight of that day in that zone. `LocalTime`, `OffsetTime`,
`OffsetDateTime` and `ZonedDateTime` are stored as ISO-8601 TEXT instead (see @ref:[Date and Time types](datetimetypes.md)).

SQLite's own date and time functions (`date`, `time`, `datetime`, `julianday`, `strftime`, `unixepoch`)
expect ISO-8601 text, Julian day numbers, or Unix time in **seconds**, so they cannot use the millisecond
values directly. Divide by 1000 and add the `'unixepoch'` modifier instead, for example
`datetime(created_at / 1000, 'unixepoch')`, which yields the UTC date and time as text.

If you want date and time values to be stored as seconds, which SQLite's functions understand directly
(`datetime(created_at, 'unixepoch')`), set the sqlite-jdbc connection property `date_precision` to `SECONDS`
in the `properties` block of the database configuration. This drops the sub-second part of the values.
The driver also has a `date_class` property (`INTEGER` (the default), `TEXT` or `REAL`) to store the values
as text (in the format given by `date_string_format`, by default `yyyy-MM-dd HH:mm:ss.SSS`) or as Julian
day numbers.

@@snip [application.conf](../code/application.conf) { #sqlite }

@@@ warning
These properties change the format of the stored data. They do not convert values already stored in an
existing database, and those values are then also read back wrongly: after switching to `date_precision = SECONDS`,
a millisecond value from 2020 reads as a date in the year 51972. Migrate the existing values first, for example
with `update t set ts = ts / 1000` when switching from milliseconds to seconds. Do this only for the columns stored
as INTEGER milliseconds (`java.sql.Date`, `Time`, `Timestamp`, `LocalDate`, `LocalDateTime` and `Instant`); the
ISO-8601 TEXT columns (`LocalTime`, `OffsetTime`, `OffsetDateTime` and `ZonedDateTime`) are not affected and must
be left unchanged.
The properties apply only to values passed as bind parameters. Slick always renders date and
time **literals** as milliseconds since the epoch. With `date_precision = SECONDS` or a `date_class` other than
`INTEGER`, a query such as `table.filter(_.createdAt === timestamp)`, where the value is inlined as a literal,
does not match the stored values. Use `timestamp.bind` or a @ref:[compiled query](queries.md#compiled-queries)
so that the value is sent as a parameter. The same applies to `O.Default` values of these types, because they
are rendered as literals in the DDL.
@@@

#### Generic JDBC

Sometimes a database system provides a JDBC driver, but no specific profile for its dialect has been implemented
for Slick, yet. It is still possible to connect to such a database with not much effort and use the generic JDBC
feature set.

Here is an example for setting up a connection to a Databricks DB using the Databricks JDBC driver, depended on via SBT:
```scala
libraryDependencies += "com.databricks" % "databricks-jdbc" % "2.6.29"
```

To configure the connection a profile implementation is required. It is sufficient to extend the `JdbcProfile` trait with an
empty object.

@@snip [GenericJdbcProfile.scala](../code/GenericJdbcProfile.scala) { #genericJdbcProfile }

Now this profile can be used in the configuration.

@@snip [application.conf](../code/application.conf) { #databricks_db }

Connection parameters for database systems can differ significantly.
Please refer to the respective documentation.

### Using a JDBC URL

You can pass a JDBC URL to
@scaladoc[forURL](slick.jdbc.DatabaseConfig$#forURL[P%3C:slick.jdbc.JdbcProfile](profile:P,url:String,user:String,password:String,prop:java.util.Properties,driver:String,keepAliveConnection:Boolean,classLoader:ClassLoader):slick.basic.BasicDatabaseConfig[profile.type]).
(see your database's JDBC driver's documentation for the correct URL syntax).

@@snip [Connection.scala](../code/Connection.scala) { #forURL }

Here we are connecting to a new, empty, in-memory H2 database called `test1` and keep it resident
until the JVM ends (`DB_CLOSE_DELAY=-1`, which is H2 specific).

@@@ note { title="Scala 2.12: passing a profile instance" }
When you pass a concrete profile object directly — the common case — everything works as expected
on all Scala versions:

```scala
DatabaseConfig.forURL(H2Profile, url, driver = "org.h2.Driver")
```

If the profile is a type parameter (e.g. `def foo[P <: JdbcProfile](p: P)`), Scala 2.12's type
inference loses track of the exact profile type when default arguments are involved. In that case,
assign the profile to a local `val` with an explicit singleton type and pass all arguments
explicitly:

```scala
val p: profile.type = profile
val dc: DatabaseConfig[p.type] = DatabaseConfig.forURL(p, url, null, null, null, driver, false, ClassLoaderUtil.defaultClassLoader)
dc.profile.backend.makeDatabase[IO](dc).unsafeRunSync()
```

On Scala 2.13 and 3.x the simpler form works fine even with a type parameter profile.
@@@

### Using a Database URL

A Database URL, a platform independent URL in the form `vendor://user:password@host:port/db`, is often provided by
platforms such as Heroku. You can use a Database URL in Typesafe Config as shown here:

@@snip [application.conf](../code/application.conf) { #dburl }

By default, the data source will use the value of the `DATABASE_URL` environment variable. Thus you may omit the `url`
property if the `DATABASE_URL` environment variable is set. You may also define a custom environment variable with
standard Typesafe Config syntax, such as `${?MYSQL_DATABASE_URL}`.

Or you may pass a @scaladoc[DatabaseUrlDataSource](slick.jdbc.DatabaseUrlDataSource) object to
@scaladoc[forDataSource](slick.jdbc.DatabaseConfig$#forDataSource[P%3C:slick.jdbc.JdbcProfile](profile:P,ds:javax.sql.DataSource,keepAliveConnection:Boolean):slick.jdbc.JdbcDatabaseConfig[P])
.

@@snip [Connection.scala](../code/Connection.scala) { #forDatabaseURL }

### Using a DataSource

You can pass a @javadoc[DataSource](javax.sql.DataSource) object to
@scaladoc[forDataSource](slick.jdbc.DatabaseConfig$#forDataSource[P%3C:slick.jdbc.JdbcProfile](profile:P,ds:javax.sql.DataSource,keepAliveConnection:Boolean):slick.jdbc.JdbcDatabaseConfig[P]).
If you got it from the connection pool of your application framework, this plugs the pool into Slick. If the pool has
a size limit, pass the size via `.withControls`:

@@snip [Connection.scala](../code/Connection.scala) { #forDataSource }

### Using a JNDI Name

If you are using @extref[JNDI](wikipedia:JNDI) you can pass a JNDI name to
@scaladoc[forName](slick.jdbc.DatabaseConfig$#forName[P%3C:slick.jdbc.JdbcProfile](profile:P,name:String):slick.jdbc.JdbcDatabaseConfig[P])
under which a @javadoc[DataSource](javax.sql.DataSource) object can be looked up. If the data source has
a limit in the number of connections it can provide, pass the size via `.withControls`:

@@snip [Connection.scala](../code/Connection.scala) { #forName }

Opening a Database
------------------

Once you have a `DatabaseConfig`, open a `Database` using the facade for your effect system.
All three facades expose the same operations: `run` for materialized query execution,
`stream` for streaming, and `close` for unmanaged lifecycles.

### Cats Effect / FS2 (`slick.cats.Database`)

The Cats facade is part of the core `slick` module.

Use `Database.resource` for managed lifecycle (recommended):

@@snip [Connection.scala](../code/Connection.scala) { #catsManaged }

If you need manual lifecycle control, use `Database.make` and call `close()` yourself:

@@snip [Connection.scala](../code/Connection.scala) { #catsUnmanaged }

Run and stream queries:

@@snip [Connection.scala](../code/Connection.scala) { #materialize }

@@snip [Connection.scala](../code/Connection.scala) { #stream }

### Future / Reactive Streams (`slick.future.Database`)

Add the Future facade module:

@@@ vars
```scala
libraryDependencies += "com.typesafe.slick" %% "slick-future" % "$project.version$"
```
@@@

Use `Database.use` for managed lifecycle (recommended):

@@snip [ConnectingFuture.scala](../code/ConnectingFuture.scala) { #futureManaged }

If you need manual lifecycle control, use `Database.open` and call `close()` yourself:

@@snip [ConnectingFuture.scala](../code/ConnectingFuture.scala) { #futureUnmanaged }

Run and stream queries:

@@snip [ConnectingFuture.scala](../code/ConnectingFuture.scala) { #futureRun }

@@snip [ConnectingFuture.scala](../code/ConnectingFuture.scala) { #futureStream }

### ZIO (`slick.zio.Database`)

Add the ZIO facade module:

@@@ vars
```scala
libraryDependencies += "com.typesafe.slick" %% "slick-zio" % "$project.version$"
```
@@@

Use `Database.scoped` for managed lifecycle (recommended):

@@snip [ConnectingZio.scala](../code/ConnectingZio.scala) { #zioManaged }

If you need manual lifecycle control, use `Database.make` and call `close()` yourself:

@@snip [ConnectingZio.scala](../code/ConnectingZio.scala) { #zioUnmanaged }

Run and stream queries:

@@snip [ConnectingZio.scala](../code/ConnectingZio.scala) { #zioRun }

@@snip [ConnectingZio.scala](../code/ConnectingZio.scala) { #zioStream }

### Action listeners

An @scaladoc[ActionListener](slick.basic.ActionListener) wraps the execution of every action, for example for
tracing or timing. Synchronous actions combined with `>>`, `DBIO.seq`, `zip`, `asTry` and similar combinators are
fused into one action, which the listener sees as one node. For `db.stream`, the listener wraps opening the stream,
not reading the rows. By default, Slick uses a listener that writes to the `slick.basic.BasicBackend.action` and
`slick.basic.BasicBackend.timing` @ref:[loggers](config.md#logging) when they are enabled at debug level.

To use your own listener instead, create the database with `makeDatabase` on the profile's backend and wrap it with
`fromCore` of your facade (`slick.cats.Database`, `slick.future.Database` or `slick.zio.Database`). You are then
responsible for closing it. `ActionListener.combine(first, second)` installs several listeners, the first one being
the outermost; combine your listener with `ActionListener.timing` to keep the `slick.basic.BasicBackend.timing` log:

```scala
import cats.effect.{IO, Resource}
import slick.basic.ActionListener

val listener = ActionListener.combine(myListener, ActionListener.timing[IO])
val database: Resource[IO, slick.cats.Database] =
  Resource.make(dc.profile.backend.makeDatabase[IO](dc, listener).map(slick.cats.Database.fromCore))(db =>
    IO.blocking(db.close()))
```

Connection pools
----------------

Using a connection pool is always recommended in production environments. Slick integrates with
@extref[HikariCP](hikaricp:) via the `slick-hikaricp` module and will use it automatically when it is on the
classpath (see @ref:[Getting Started](gettingstarted.md) for the dependency).

Set the *maximum* pool size to the number of concurrent database connections your database server can handle
efficiently. Because Slick's execution model is non-blocking, you do not need to add extra connections to
"absorb" thread blocking — size the pool purely based on the database server's capacity. See
@extref[About Pool Sizing](about-pool-sizing:) in the HikariCP documentation for background.

Note that reasonable defaults for the connection pool sizes are calculated automatically when using
@scaladoc[DatabaseConfig.forConfig](slick.basic.DatabaseConfig$#forConfig[P](path:String,config:com.typesafe.config.Config,classLoader:ClassLoader)(implicitP:scala.reflect.ClassTag[P]):slick.basic.BasicDatabaseConfig[P]).

Slick uses *prepared* statements wherever possible but it does not cache them on its own. You
should therefore enable prepared statement caching in the connection pool's configuration.

### HikariCP settings

With `connectionPool = "HikariCP"` (the default), the pool is configured from the same config section
as the database. Most keys have the same names as the
[HikariCP configuration properties](https://github.com/brettwooldridge/HikariCP#gear-configuration-knobs-baby) and are passed on to
HikariCP. These keys need some explanation:

- `dataSourceClass` or `dataSourceClassName`: the `javax.sql.DataSource` class provided by the JDBC
  driver, configured with `properties`. Without it, the pool uses `driver` (or `driverClassName`) and
  `url` (or `jdbcUrl`). Slick fails with a `SlickException` when the class is not a `DataSource`.
- `maximumPoolSize` or `maxConnections` (default: `numThreads`, or 20): the maximum pool size. Slick also
  limits concurrent JDBC work to the pool size. If both keys are set, `maximumPoolSize` is used, and if
  their values differ, Slick logs a warning, because a `maximumPoolSize` in a `reference.conf` file would otherwise silently override
  your `maxConnections`.
- `minimumIdle` or `minConnections` (default: `maxConnections`, or `numThreads`, or 20): the minimum
  number of idle connections.
- `initializationFailTimeout` (default: `-1`): what happens when the pool cannot get a first connection
  while starting. A positive value is the time to try (in milliseconds, or a duration such as `5s`) before
  the pool fails to start; `1` makes the pool fail fast. With `0` the pool starts even if it cannot get
  a connection, and with `-1` it does not try to get one. Note that Slick's default (`-1`) differs from
  HikariCP's (`1`).

#### Pool metrics

To register metrics or health checks on the HikariCP pool, write a
@scaladoc[JdbcDataSourceFactory](slick.jdbc.JdbcDataSourceFactory) object that gets the pool configuration
from @scaladoc[HikariCPJdbcDataSource.hikariConfig](slick.jdbc.hikaricp.HikariCPJdbcDataSource$), sets them
on it and then creates the pool:

```scala
object InstrumentedHikariCP extends JdbcDataSourceFactory {
  def forConfig(c: Config, driver: Driver, name: String, classLoader: ClassLoader): HikariCPJdbcDataSource = {
    val hconf = HikariCPJdbcDataSource.hikariConfig(c, name, classLoader)
    hconf.setMetricRegistry(myMetricRegistry) // or setMetricsTrackerFactory
    hconf.setHealthCheckRegistry(myHealthCheckRegistry)
    new HikariCPJdbcDataSource(new HikariDataSource(hconf), hconf)
  }
}
```

Name that object in the configuration with `connectionPool = "com.example.InstrumentedHikariCP$"`.

Execution Concurrency Model
---------------------------

Slick 4 uses a two-stage admission model for `db.run(...)` and `db.stream(...)` calls:

1. **Queue** (`queueSize`): bounds how many callers are allowed to wait for admission.
2. **In-flight** (`maxInflightActions`): bounds how many DBIO chains may run concurrently.

After a call is admitted, actual JDBC work is still bounded by the connection pool size
(`maxConnections`).

### Why these two limits exist

- **Queue** protects the application from unbounded memory growth under overload.
- **In-flight** limits total concurrent action chains (including chains that are between DB steps).
- **Connections** limit actual simultaneous JDBC I/O.

This separation avoids a common overload pattern where too many callers pile up waiting forever,
while still allowing some headroom when actions do non-DB work between JDBC steps.

### Configuration keys

These keys are read from the same database config section used by `DatabaseConfig.forConfig` /
`forProfileConfig`. For programmatic configs (`forDataSource`, `forSource`, `forName`, `forURL`)
use @scaladoc[ControlsConfig](slick.ControlsConfig) and `.withControls` instead:

```scala
DatabaseConfig.forDataSource(MyProfile, ds)
  .withControls(ControlsConfig(maxConnections = 10, queueSize = 500))
```

Config-file keys (for `forConfig` / `forProfileConfig`):

- `maxConnections`: maximum concurrent JDBC connections (existing key).
- `queueSize`: maximum number of callers waiting for in-flight admission (default: `1000`).
- `maxInflightActions`: maximum concurrently running DBIO chains (default: `2 * maxConnections`).
- `inflightAdmissionTimeout`: optional timeout while waiting for in-flight admission.
- `connectionAcquireTimeout`: optional timeout while waiting for a connection slot.

If `maxInflightActions` is configured lower than `maxConnections`, Slick uses
`maxConnections` as the effective value.

Example:

```hocon
mydb {
  connectionPool = "HikariCP"

  # JDBC concurrency
  maxConnections = 10

  # DBIO admission control
  maxInflightActions = 20
  queueSize = 1000

  # Optional timeout controls
  inflightAdmissionTimeout = 2s
  connectionAcquireTimeout = 5s
}
```

Timeout failures are returned as `SlickException`:

- `SlickException("Timed out waiting for inflight admission after ...")`
- `SlickException("Timed out waiting for connection slot after ...")`

### Overload behavior

When the queue is full, new calls fail fast with:

- `SlickException("DBIOAction queue full")`

This is intentional back-pressure. Typical responses are to retry with jittered backoff, shed load,
or increase capacity (`queueSize`, `maxInflightActions`, and/or database connection pool size)
based on measured behavior.
