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
@scaladoc[API documentation](slick.basic.DatabaseConfig$#forProfileConfig[P](profile:P,path:String,config:com.typesafe.config.Config,classLoader:ClassLoader):slick.basic.BasicDatabaseConfig[profile.type])
of this method for details on the configuration parameters). It returns a `DatabaseConfig` value
that you can open with any of the three facades.

@@snip [Connection.scala](../code/Connection.scala) { #forConfig }

#### PostgreSQL
@@snip [application.conf](../code/application.conf) { #postgres }

When specifying a dataSourceClass you will need to bring in the sbt dependency for that class. The following is an example for the `org.postgresql.ds.PGSimpleDataSource` data source class:

@@@ vars
```scala
libraryDependencies ++= Seq(
  "com.typesafe.slick" %% "slick" % "$project.version$",
  "org.slf4j" % "slf4j-nop" % "1.7.26",
  "com.typesafe.slick" %% "slick-hikaricp" % "$project.version$",
  "org.postgresql" % "postgresql" % "42.2.5" //org.postgresql.ds.PGSimpleDataSource dependency
)
```
@@@

Note, some examples on the internet point to the 9.4.X versions of the
PostgreSQL JDBC drivers. Those examples are incorrect and many years out of
date. The 42.X development line has been the main line of development since
February 2017. As noted on the Postgres site, "Unless you have unusual
requirements (running old applications or JVMs), this is the driver you should
be using." See https://jdbc.postgresql.org/download.html for more information
about when you may want to use the 9.4 line.

#### MySQL

Very simple example without a connection pool and using the driver directly:

@@snip [application.conf](../code/application.conf) { #mysql }

To use the MySQL driver, the following library dependency needs to be configured via SBT:

```scala
libraryDependencies += "com.mysql" % "mysql-connector-j" % "8.0.33"
```

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
@scaladoc[forDataSource](slick.jdbc.DatabaseConfig$#forDataSource[P%3C:slick.jdbc.JdbcProfile](profile:P,ds:javax.sql.DataSource,maxConnections:scala.Option[Int],keepAliveConnection:Boolean,classLoader:ClassLoader):slick.basic.BasicDatabaseConfig[profile.type])
.

@@snip [Connection.scala](../code/Connection.scala) { #forDatabaseURL }

### Using a DataSource

You can pass a @javadoc[DataSource](javax.sql.DataSource) object to
@scaladoc[forDataSource](slick.jdbc.DatabaseConfig$#forDataSource[P%3C:slick.jdbc.JdbcProfile](profile:P,ds:javax.sql.DataSource,maxConnections:scala.Option[Int],keepAliveConnection:Boolean,classLoader:ClassLoader):slick.basic.BasicDatabaseConfig[profile.type]).
If you got it from the connection pool of your application framework, this plugs the pool into Slick. If the pool has
a size limit, the correct size should always be specified.

@@snip [Connection.scala](../code/Connection.scala) { #forDataSource }

### Using a JNDI Name

If you are using @extref[JNDI](wikipedia:JNDI) you can pass a JNDI name to
@scaladoc[forName](slick.jdbc.DatabaseConfig$#forName[P%3C:slick.jdbc.JdbcProfile](profile:P,name:String,maxConnections:scala.Option[Int],classLoader:ClassLoader):slick.basic.BasicDatabaseConfig[profile.type])
under which a @javadoc[DataSource](javax.sql.DataSource) object can be looked up. If the data source has
a limit in the number of connections it can provide, the correct size should always be specified.

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

These keys are read from the same database config section used by `Database.forConfig`:

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
