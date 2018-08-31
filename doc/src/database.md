Database Configuration
======================

You can tell Slick how to connect to the JDBC database of your choice by creating a
[Database](api:slick.jdbc.JdbcBackend@Database:Database) object, which encapsulates the information. There are several
[factory methods](api:slick.jdbc.JdbcBackend$DatabaseFactoryDef) on `slick.jdbc.JdbcBackend.Database` that you can use
depending on what connection data you have available.

Using Typesafe Config
---------------------

The preferred way to configure database connections is through [Typesafe Config] in your
`application.conf`, which is also used by [Play] and [Akka] for their configuration.

For Postgres: 
```yaml src=../code/application.conf#postgres
```

When specifying a dataSourceClass you will need to bring in the sbt dependency for that class. The following is an example for the `org.postgresql.ds.PGSimpleDataSource` data source class:

```scala expandVars=true
libraryDependencies ++= Seq(
  "com.typesafe.slick" %% "slick" % "{{version}}",
  "org.slf4j" % "slf4j-nop" % "1.6.4",
  "com.typesafe.slick" %% "slick-hikaricp" % "{{version}}",
  "org.postgresql" % "postgresql" % "9.4-1206-jdbc42" //org.postgresql.ds.PGSimpleDataSource dependency
)
``` 

Such a configuration can be loaded with `Database.forConfig` (see the
[API documentation](api:slick.jdbc.JdbcBackend$DatabaseFactoryDef@forConfig(String,Config,Driver,ClassLoader):Database)
of this method for details on the configuration parameters).

```scala src=../code/Connection.scala#forConfig
```

Using a JDBC URL {index=URL}
----------------

You can pass a JDBC URL to
<api:slick.jdbc.JdbcBackend$DatabaseFactoryDef@forURL(String,String,String,Properties,String,AsyncExecutor,Boolean,ClassLoader):DatabaseDef>.
(see your database's JDBC driver's documentation for the correct URL syntax).

```scala src=../code/Connection.scala#forURL
```

Here we are connecting to a new, empty, in-memory H2 database called `test1` and keep it resident
until the JVM ends (`DB_CLOSE_DELAY=-1`, which is H2 specific).

Using a Database URL {index=DataSource}
--------------------

A Database URL, a platform independent URL in the form `vendor://user:password@host:port/db`, is often provided by
platforms such as Heroku. You can use a Database URL in Typesafe Config as shown here:

```yaml src=../code/application.conf#dburl
```

By default, the data source will use the value of the `DATABASE_URL` environment variable. Thus you may omit the `url`
property if the `DATABASE_URL` environment variable is set. You may also define a custom environment variable with
standard Typesafe Config syntax, such as `${?MYSQL_DATABASE_URL}`.

Or you may pass a <api:slick/jdbc/DatabaseUrlDataSource> object to
<api:slick.jdbc.JdbcBackend$DatabaseFactoryDef@forDataSource(DataSource,Option[Int],AsyncExecutor,Boolean):DatabaseDef>.

```scala src=../code/Connection.scala#forDatabaseURL
```

Using a DataSource
------------------

You can pass a [DataSource](javaapi:javax/sql/DataSource) object to
<api:slick.jdbc.JdbcBackend$DatabaseFactoryDef@forDataSource(DataSource,Option[Int],AsyncExecutor,Boolean):DatabaseDef>.
If you got it from the connection pool of your application framework, this plugs the pool into Slick. If the pool has
a size limit, the correct size should always be specified.

```scala src=../code/Connection.scala#forDataSource
```

Using a JNDI Name {index=JNDI}
-----------------

If you are using [JNDI](wikipedia:JNDI) you can pass a JNDI name to
<api:slick.jdbc.JdbcBackend$DatabaseFactoryDef@forName(String,Option[Int],AsyncExecutor):DatabaseDef>
under which a [DataSource](javaapi:javax/sql/DataSource) object can be looked up. If the data source has
a limit in the number of connections it can provide, the correct size should always be specified.

```scala src=../code/Connection.scala#forName
```

Database thread pool {index="thread,pool;pool,thread"}
--------------------

Every `Database` contains an <api:slick.util.AsyncExecutor> that manages the thread pool
for asynchronous execution of Database I/O Actions. Its size is the main parameter to tune for the best
performance of the `Database` object. It should be set to the value that you would use for the
size of the *connection pool* in a traditional, blocking application (see [About Pool Sizing]
in the [HikariCP] documentation for further information). When using
[Database.forConfig](api:slick.jdbc.JdbcBackend$DatabaseFactoryDef@forConfig(String,Config,Driver,ClassLoader):Database),
the thread pool is configured directly in the external configuration file together with the connection
parameters. If you use any other factory method to get a `Database`, you can either use a default
configuration or specify a custom AsyncExecutor:

```scala src=../code/Connection.scala#forURL2
```

Connection pools {index="connection,pool;pool,connection"}
----------------

When using a connection pool (which is always recommended in production environments) the *minimum*
size of the *connection pool* should also be set to at least the same size. The *maximum* size of
the *connection pool* can be set much higher than in a blocking application. Any connections beyond
the size of the *thread pool* will only be used when other connections are required to keep a
database session open (e.g. while waiting for the result from an asynchronous computation in the
middle of a transaction) but are not actively doing any work on the database.

Note that reasonable defaults for the connection pool sizes are calculated from the thread pool size when using
[Database.forConfig](api:slick.jdbc.JdbcBackend$DatabaseFactoryDef@forConfig(String,Config,Driver,ClassLoader):Database).

Slick uses *prepared* statements wherever possible but it does not cache them on its own. You
should therefore enable prepared statement caching in the connection pool's configuration.

DatabaseConfig
--------------

> {.note}
> This section is based on the **MultiDB** sample ([github](samplerepo:slick-multidb),
> [zip](samplezip:slick-multidb)) which provides ready-to-run apps to demonstrate the features.

On top of the configuration syntax for `Database`, there is another layer in the form of
<api:slick.basic.DatabaseConfig> which allows you to configure a Slick profile plus a
matching `Database` together. This makes it easy to abstract over different kinds of
database systems by simply changing a configuration file.

You can see it in action in `SimpleExample.scala`. First we load the DatabaseConfig and then import the
Slick API from its profile:

```scala src=../../samples/slick-multidb/src/main/scala/databaseconfig/SimpleExample.scala#dc
```

The `JdbcProfile` type annotation specifies the profile level whose API you get. You have to configure a profile of
a matching type in the external configuration file. Since we're using the basic `forConfig` method with only a path
("h2_dc"), the configuration must be located in the global application config, usually found in `application.conf`:

```scala src=../../samples/slick-multidb/src/main/resources/application.conf#--doc-h2_db
```

You can use different database systems in your application by either switching out or overriding the application
config (e.g. different `application.conf` files for development and production) or by passing a config path into
the application. This way of implementing multi-database support is also used when building a Play app with Slick.

Other Multi-DB Patterns
-----------------------

> {.note}
> This section is based on the **MultiDB** sample ([github](samplerepo:slick-multidb),
> [zip](samplezip:slick-multidb)) which provides ready-to-run apps to demonstrate the features.

Since its addition in Slick 3.0 `DatabaseConfig` (see [above](#databaseconfig)) is the recommended solution.
More complex scenarios (for example, where you need to map custom functions differently for different database
systems, or where you cannot use the simple application.conf syntax) may require abstracting over databases in Scala
code. The following sections explain two different ways of accomplishing this.

### A DAO Class

We start with a simple DAO (data access object) class, `DAO.scala`. It contains some database-related definitions
(for a `PROPS` table that acts as a simple key/value store) and methods (for storing and reading entries).

The class is parameterized by a concrete `JdbcProfile` and it imports all API features from this profile's api object:

```scala src=../../samples/slick-multidb/src/main/scala/basic/DAO.scala#dao
```

Slick has multiple abstract profiles but in most cases you will want to use `JdbcProfile` which contains all the
features that you also get when importing directly from one of Slick's concrete profiles for JDBC databases.

Outside of the DAO class, you can still refer to its profile and the other features, but you have to get the imports
from the profile in order to work with queries. This can be seen in `DAOHelper.scala`. It defines a new method
`restrictKey` which we could have also put directly into the DAO class.

To gain access to all of the profile's features, we parameterize the `DAOHelper` with the `DAO` and import from its
profile:

```scala src=../../samples/slick-multidb/src/main/scala/basic/DAOHelper.scala#daohelper
```

Note the use of the type projection `DAO#Props` in the definition of `restrictKey`. This points to the `Props` type
coming from any `DAO` instance. This is less type-safe than using a path-dependent type like `dao.Props` but
generally easier to use. You still need to ensure not to mix drivers when you do this.

We are using the `DAO` and `DAOHelper` in `MultiDBExample.scala`. The `run` method is parameterized with both,
a Slick profile and a matching JDBC `Database`:

```scala src=../../samples/slick-multidb/src/main/scala/basic/MultiDBExample.scala#run
```

Since we don't have the convenience of a single profile's `api._` import at this point, we need to import the
`Database` and `DBIO` types directly:

```scala src=../../samples/slick-multidb/src/main/scala/basic/MultiDBExample.scala#imports
```

In the body of `MultiDBExample`, we create two `DAO` instances with matching `Database` objects in order to run the
same code on both, H2 and SQLite:

```scala src=../../samples/slick-multidb/src/main/scala/basic/MultiDBExample.scala#create
```

### The Cake Pattern

In more complex designs where you want to split up the database code into separate modules that deal with different
database entities, but still have dependencies between these modules, you can use the Cake Pattern to structure your
code.

We are doing this here in the app `MultiDBCakeExample.scala`. From the point of view of this main app, the new
approach looks exactly like the previous one: You create a DAL (data access layer) object with a Slick profile, and
use it together with a matching `Database`.

The most basic slice of the cake is the `ProfileComponent`. It provides a `JdbcProfile` which is kept abstract at
this point:

```scala src=../../samples/slick-multidb/src/main/scala/cake/ProfileComponent.scala
```

Through the use of a self-type, the `PictureComponent` requires a `ProfileComponent` to me mixed in, so that it can
import the query language features from the profile:

```scala src=../../samples/slick-multidb/src/main/scala/cake/PictureComponent.scala#outline
```

Using the imported features, `PictureComponent` provides definitions and methods for working with `Picture` objects
in the database. `UserComponent` does the same for `User` entities. In addition to `ProfileComponent` it also
requires `PictureComponent`:

```scala src=../../samples/slick-multidb/src/main/scala/cake/UserComponent.scala#outline
```

The `PictureComponent` dependency allows `UserComponent` to insert a new `Picture` when needed:

```scala src=../../samples/slick-multidb/src/main/scala/cake/UserComponent.scala#insert
```

We put all slices of the cake together in `DAL.scala`. The `DAL` class inherits from all components and adds the
missing profile through a field in the constructor:

```scala src=../../samples/slick-multidb/src/main/scala/cake/DAL.scala
```

This is also a good place to add functionality that affects all components, like the `create` method.
