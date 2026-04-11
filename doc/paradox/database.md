Database Configuration
======================

You can tell Slick how to connect to the JDBC database of your choice by creating a
@scaladoc[Database](slick.jdbc.JdbcBackend#Database:Database) object, which encapsulates the information. There are several
@scaladoc[factory methods](slick.jdbc.JdbcBackend$DatabaseFactoryDef) on `slick.jdbc.JdbcBackend.Database` that you can use
depending on what connection data you have available.

All factory methods return a `Resource[F, Database[F]]`, where `F` is your effect type (e.g. `cats.effect.IO`). The
`Resource` manages the full lifecycle of the connection pool: it is acquired when the `Resource` is used and released
(connections closed, pool shut down) when the `Resource` is released. You never need to call `close()` or `shutdown()`
manually.

Using Typesafe Config
---------------------

The preferred way to configure database connections is through @extref[Typesafe Config](typesafe-config:) in your
`application.conf`, which is also used by @extref[Play](play:) and @extref[Akka](akka:) for their configuration.

Such a configuration can be loaded with `Database.forConfig` (see the
@scaladoc[API documentation](slick.jdbc.JdbcBackend$DatabaseFactoryDef#forConfig)
of this method for details on the configuration parameters). It returns a `Resource[F, Database[F]]`.

@@snip [Connection.scala](../code/Connection.scala) { #forConfig }

#### Examples

##### PostgreSQL
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

##### MySQL

Very simple example without a connection pool and using the driver directly:

@@snip [application.conf](../code/application.conf) { #mysql }

To use the MySQL driver, the following library dependency needs to be configured via SBT:

```scala
libraryDependencies += "com.mysql" % "mysql-connector-j" % "8.0.33"
```

##### Generic JDBC
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

Using a JDBC URL
----------------

You can pass a JDBC URL to
@scaladoc[forURL](slick.jdbc.JdbcBackend$DatabaseFactoryDef#forURL).
(see your database's JDBC driver's documentation for the correct URL syntax).

@@snip [Connection.scala](../code/Connection.scala) { #forURL }

Here we are connecting to a new, empty, in-memory H2 database called `test1` and keep it resident
until the JVM ends (`DB_CLOSE_DELAY=-1`, which is H2 specific).

Using a Database URL
--------------------

A Database URL, a platform independent URL in the form `vendor://user:password@host:port/db`, is often provided by
platforms such as Heroku. You can use a Database URL in Typesafe Config as shown here:

@@snip [application.conf](../code/application.conf) { #dburl }

By default, the data source will use the value of the `DATABASE_URL` environment variable. Thus you may omit the `url`
property if the `DATABASE_URL` environment variable is set. You may also define a custom environment variable with
standard Typesafe Config syntax, such as `${?MYSQL_DATABASE_URL}`.

Or you may pass a @scaladoc[DatabaseUrlDataSource](slick.jdbc.DatabaseUrlDataSource) object to
@scaladoc[forDataSource](slick.jdbc.JdbcBackend$DatabaseFactoryDef#forDataSource)
.

@@snip [Connection.scala](../code/Connection.scala) { #forDatabaseURL }

Using a DataSource
------------------

You can pass a @javadoc[DataSource](javax.sql.DataSource) object to
@scaladoc[forDataSource](slick.jdbc.JdbcBackend$DatabaseFactoryDef#forDataSource).
If you got it from the connection pool of your application framework, this plugs the pool into Slick. If the pool has
a size limit, the correct size should always be specified.

@@snip [Connection.scala](../code/Connection.scala) { #forDataSource }

Using a JNDI Name
-----------------

If you are using @extref[JNDI](wikipedia:JNDI) you can pass a JNDI name to
@scaladoc[forName](slick.jdbc.JdbcBackend$DatabaseFactoryDef#forName)
under which a @javadoc[DataSource](javax.sql.DataSource) object can be looked up. If the data source has
a limit in the number of connections it can provide, the correct size should always be specified.

@@snip [Connection.scala](../code/Connection.scala) { #forName }

Database lifecycle
------------------

Every factory method returns a `Resource[F, Database[F]]`. The `Resource` allocates the connection pool when
acquired and shuts it down when released. The recommended pattern is to acquire the `Resource` once at
application startup and keep the `Database` value alive for the lifetime of the application:

```scala
val dbResource: Resource[IO, Database[IO]] = Database.forConfig[IO]("mydb")

// In your main entry point:
dbResource.use { db =>
  // run your entire application here
  myApp(db)
}
```

You do not need to call `close()` or any shutdown method manually — the `Resource` finalizer handles it. If you are
using a framework such as http4s or Tapir, you can convert the `Resource` into the framework's service lifecycle
using standard Cats Effect `Resource` combinators.

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
@scaladoc[Database.forConfig](slick.jdbc.JdbcBackend$DatabaseFactoryDef#forConfig).

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

DatabaseConfig
--------------

@@@ note

This section is based on the @extref[**MultiDB** sample](samplerepo:slick-multidb) which provides ready-to-run apps to
demonstrate the features.

@@@

On top of the configuration syntax for `Database`, there is another layer in the form of
@scaladoc[DatabaseConfig](slick.basic.DatabaseConfig) which allows you to configure a Slick profile plus a
matching `Database` together. This makes it easy to abstract over different kinds of
database systems by simply changing a configuration file.

You can see it in action in `SimpleExample.scala`. First we load the DatabaseConfig and then import the
Slick API from its profile:

@@snip [SimpleExample.scala](samples/slick-multidb/src/main/scala/slick/examples/multidb/databaseconfig/SimpleExample.scala) { #dc }

The `JdbcProfile` type annotation specifies the profile level whose API you get. You have to configure a profile of
a matching type in the external configuration file. Since we're using the basic `forConfig` method with only a path
("h2_dc"), the configuration must be located in the global application config, usually found in `application.conf`:

@@snip [application.conf](samples/slick-multidb/src/main/resources/application.conf) { #doc-h2_db }

You can use different database systems in your application by either switching out or overriding the application
config (e.g. different `application.conf` files for development and production) or by passing a config path into
the application. This way of implementing multi-database support is also used when building a Play app with Slick.

Other Multi-DB Patterns
-----------------------

@@@ note

This section is based on the @extref[**MultiDB** sample](samplerepo:slick-multidb) which provides ready-to-run apps to
demonstrate the features.

@@@

`DatabaseConfig` (see [above](#databaseconfig)) is the recommended solution.
More complex scenarios (for example, where you need to map custom functions differently for different database
systems, or where you cannot use the simple application.conf syntax) may require abstracting over databases in Scala
code. The following sections explain two different ways of accomplishing this.

### A DAO Class

We start with a simple DAO (data access object) class, `DAO.scala`. It contains some database-related definitions
(for a `PROPS` table that acts as a simple key/value store) and methods (for storing and reading entries).

The class is parameterized by a concrete `JdbcProfile` and it imports all API features from this profile's api object:

@@snip [DAO.scala](samples/slick-multidb/src/main/scala/slick/examples/multidb/basic/DAO.scala) { #dao }

Slick has multiple abstract profiles but in most cases you will want to use `JdbcProfile` which contains all the
features that you also get when importing directly from one of Slick's concrete profiles for JDBC databases.

Outside of the DAO class, you can still refer to its profile and the other features, but you have to get the imports
from the profile in order to work with queries. This can be seen in `DAOHelper.scala`. It defines a new method
`restrictKey` which we could have also put directly into the DAO class.

To gain access to all of the profile's features, we parameterize the `DAOHelper` with the `DAO` and import from its
profile:

@@snip [DAOHelper.scala](samples/slick-multidb/src/main/scala/slick/examples/multidb/basic/DAOHelper.scala) { #daohelper }

Note the use of the type projection `DAO#Props` in the definition of `restrictKey`. This points to the `Props` type
coming from any `DAO` instance. This is less type-safe than using a path-dependent type like `dao.Props` but
generally easier to use. You still need to ensure not to mix drivers when you do this.

We are using the `DAO` and `DAOHelper` in `MultiDBExample.scala`. The `run` method is parameterized with both,
a Slick profile and a matching JDBC `Database`:

@@snip [MultiDBExample.scala](samples/slick-multidb/src/main/scala/slick/examples/multidb/basic/MultiDBExample.scala) { #run }

Since we don't have the convenience of a single profile's `api._` import at this point, we need to import the
`Database` and `DBIO` types directly:

@@snip [MultiDBExample.scala](samples/slick-multidb/src/main/scala/slick/examples/multidb/basic/MultiDBExample.scala) { #imports }

In the body of `MultiDBExample`, we create two `DAO` instances with matching `Database` objects in order to run the
same code on both, H2 and SQLite:

@@snip [MultiDBExample.scala](samples/slick-multidb/src/main/scala/slick/examples/multidb/basic/MultiDBExample.scala) { #create }

### The Cake Pattern

In more complex designs where you want to split up the database code into separate modules that deal with different
database entities, but still have dependencies between these modules, you can use the Cake Pattern to structure your
code.

We are doing this here in the app `MultiDBCakeExample.scala`. From the point of view of this main app, the new
approach looks exactly like the previous one: You create a DAL (data access layer) object with a Slick profile, and
use it together with a matching `Database`.

The most basic slice of the cake is the `ProfileComponent`. It provides a `JdbcProfile` which is kept abstract at
this point:

@@snip [ProfileComponent.scala](samples/slick-multidb/src/main/scala/slick/examples/multidb/cake/ProfileComponent.scala)

Through the use of a self-type, the `PictureComponent` requires a `ProfileComponent` to me mixed in, so that it can
import the query language features from the profile:

@@snip [PictureComponent.scala](samples/slick-multidb/src/main/scala/slick/examples/multidb/cake/PictureComponent.scala) { #outline }

Using the imported features, `PictureComponent` provides definitions and methods for working with `Picture` objects
in the database. `UserComponent` does the same for `User` entities. In addition to `ProfileComponent` it also
requires `PictureComponent`:

@@snip [UserComponent.scala](samples/slick-multidb/src/main/scala/slick/examples/multidb/cake/UserComponent.scala) { #outline }

The `PictureComponent` dependency allows `UserComponent` to insert a new `Picture` when needed:

@@snip [UserComponent.scala](samples/slick-multidb/src/main/scala/slick/examples/multidb/cake/UserComponent.scala) { #insert }

We put all slices of the cake together in `DAL.scala`. The `DAL` class inherits from all components and adds the
missing profile through a field in the constructor:

@@snip [DAL.scala](samples/slick-multidb/src/main/scala/slick/examples/multidb/cake/DAL.scala)

This is also a good place to add functionality that affects all components, like the `create` method.
