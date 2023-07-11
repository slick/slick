Database Configuration
======================

You can tell Slick how to connect to the JDBC database of your choice by creating a
@scaladoc[Database](slick.jdbc.JdbcBackend#Database:Database) object, which encapsulates the information. There are several
@scaladoc[factory methods](slick.jdbc.JdbcBackend$DatabaseFactoryDef) on `slick.jdbc.JdbcBackend.Database` that you can use
depending on what connection data you have available.

Using Typesafe Config
---------------------

The preferred way to configure database connections is through @extref[Typesafe Config](typesafe-config:) in your
`application.conf`, which is also used by @extref[Play](play:) and @extref[Akka](akka:) for their configuration.

Such a configuration can be loaded with `Database.forConfig` (see the
@scaladoc[API documentation](slick.jdbc.JdbcBackend$DatabaseFactoryDef#forConfig(String,Config,Driver,ClassLoader):Database)
of this method for details on the configuration parameters).

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
@scaladoc[forURL](slick.jdbc.JdbcBackend$DatabaseFactoryDef#forURL(String,String,String,Properties,String,AsyncExecutor,Boolean,ClassLoader):DatabaseDef).
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
@scaladoc[forDataSource](slick.jdbc.JdbcBackend$DatabaseFactoryDef#forDataSource(DataSource,Option[Int],AsyncExecutor,Boolean):DatabaseDef)
.

@@snip [Connection.scala](../code/Connection.scala) { #forDatabaseURL }

Using a DataSource
------------------

You can pass a @javadoc[DataSource](javax.sql.DataSource) object to
@scaladoc[forDataSource](slick.jdbc.JdbcBackend$DatabaseFactoryDef#forDataSource(DataSource,Option[Int],AsyncExecutor,Boolean):DatabaseDef).
If you got it from the connection pool of your application framework, this plugs the pool into Slick. If the pool has
a size limit, the correct size should always be specified.

@@snip [Connection.scala](../code/Connection.scala) { #forDataSource }

Using a JNDI Name
-----------------

If you are using @extref[JNDI](wikipedia:JNDI) you can pass a JNDI name to
@scaladoc[forName](slick.jdbc.JdbcBackend$DatabaseFactoryDef#forName(String,Option[Int],AsyncExecutor):DatabaseDef)
under which a @javadoc[DataSource](javax.sql.DataSource) object can be looked up. If the data source has
a limit in the number of connections it can provide, the correct size should always be specified.

@@snip [Connection.scala](../code/Connection.scala) { #forName }

Database thread pool
--------------------

Every `Database` contains an @scaladoc[AsyncExecutor](slick.util.AsyncExecutor) that manages the thread pool
for asynchronous execution of Database I/O Actions. Its size is the main parameter to tune for the best
performance of the `Database` object. It should be set to the value that you would use for the
size of the *connection pool* in a traditional, blocking application (see @extref[About Pool Sizing](about-pool-sizing:)
in the @extref[HikariCP](hikaricp:) documentation for further information). When using
@scaladoc[Database.forConfig](slick.jdbc.JdbcBackend$DatabaseFactoryDef#forConfig(String,Config,Driver,ClassLoader):Database),
the thread pool is configured directly in the external configuration file together with the connection
parameters. If you use any other factory method to get a `Database`, you can either use a default
configuration or specify a custom AsyncExecutor:

@@snip [Connection.scala](../code/Connection.scala) { #forURL2 }

Connection pools
----------------

When using a connection pool (which is always recommended in production environments) the *minimum*
size of the *connection pool* should also be set to at least the same size. The *maximum* size of
the *connection pool* can be set much higher than in a blocking application. Any connections beyond
the size of the *thread pool* will only be used when other connections are required to keep a
database session open (e.g. while waiting for the result from an asynchronous computation in the
middle of a transaction) but are not actively doing any work on the database.

Note that reasonable defaults for the connection pool sizes are calculated from the thread pool size when using
@scaladoc[Database.forConfig](slick.jdbc.JdbcBackend$DatabaseFactoryDef#forConfig(String,Config,Driver,ClassLoader):Database).

Slick uses *prepared* statements wherever possible but it does not cache them on its own. You
should therefore enable prepared statement caching in the connection pool's configuration.

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

@@snip [SimpleExample.scala](samples/slick-multidb/src/main/scala/databaseconfig/SimpleExample.scala) { #dc }

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

Since its addition in Slick 3.0 `DatabaseConfig` (see [above](#databaseconfig)) is the recommended solution.
More complex scenarios (for example, where you need to map custom functions differently for different database
systems, or where you cannot use the simple application.conf syntax) may require abstracting over databases in Scala
code. The following sections explain two different ways of accomplishing this.

### A DAO Class

We start with a simple DAO (data access object) class, `DAO.scala`. It contains some database-related definitions
(for a `PROPS` table that acts as a simple key/value store) and methods (for storing and reading entries).

The class is parameterized by a concrete `JdbcProfile` and it imports all API features from this profile's api object:

@@snip [DAO.scala](samples/slick-multidb/src/main/scala/basic/DAO.scala) { #dao }

Slick has multiple abstract profiles but in most cases you will want to use `JdbcProfile` which contains all the
features that you also get when importing directly from one of Slick's concrete profiles for JDBC databases.

Outside of the DAO class, you can still refer to its profile and the other features, but you have to get the imports
from the profile in order to work with queries. This can be seen in `DAOHelper.scala`. It defines a new method
`restrictKey` which we could have also put directly into the DAO class.

To gain access to all of the profile's features, we parameterize the `DAOHelper` with the `DAO` and import from its
profile:

@@snip [DAOHelper.scala](samples/slick-multidb/src/main/scala/basic/DAOHelper.scala) { #daohelper }

Note the use of the type projection `DAO#Props` in the definition of `restrictKey`. This points to the `Props` type
coming from any `DAO` instance. This is less type-safe than using a path-dependent type like `dao.Props` but
generally easier to use. You still need to ensure not to mix drivers when you do this.

We are using the `DAO` and `DAOHelper` in `MultiDBExample.scala`. The `run` method is parameterized with both,
a Slick profile and a matching JDBC `Database`:

@@snip [MultiDBExample.scala](samples/slick-multidb/src/main/scala/basic/MultiDBExample.scala) { #run }

Since we don't have the convenience of a single profile's `api._` import at this point, we need to import the
`Database` and `DBIO` types directly:

@@snip [MultiDBExample.scala](samples/slick-multidb/src/main/scala/basic/MultiDBExample.scala) { #imports }

In the body of `MultiDBExample`, we create two `DAO` instances with matching `Database` objects in order to run the
same code on both, H2 and SQLite:

@@snip [MultiDBExample.scala](samples/slick-multidb/src/main/scala/basic/MultiDBExample.scala) { #create }

### The Cake Pattern

In more complex designs where you want to split up the database code into separate modules that deal with different
database entities, but still have dependencies between these modules, you can use the Cake Pattern to structure your
code.

We are doing this here in the app `MultiDBCakeExample.scala`. From the point of view of this main app, the new
approach looks exactly like the previous one: You create a DAL (data access layer) object with a Slick profile, and
use it together with a matching `Database`.

The most basic slice of the cake is the `ProfileComponent`. It provides a `JdbcProfile` which is kept abstract at
this point:

@@snip [ProfileComponent.scala](samples/slick-multidb/src/main/scala/cake/ProfileComponent.scala)

Through the use of a self-type, the `PictureComponent` requires a `ProfileComponent` to me mixed in, so that it can
import the query language features from the profile:

@@snip [PictureComponent.scala](samples/slick-multidb/src/main/scala/cake/PictureComponent.scala) { #outline }

Using the imported features, `PictureComponent` provides definitions and methods for working with `Picture` objects
in the database. `UserComponent` does the same for `User` entities. In addition to `ProfileComponent` it also
requires `PictureComponent`:

@@snip [UserComponent.scala](samples/slick-multidb/src/main/scala/cake/UserComponent.scala) { #outline }

The `PictureComponent` dependency allows `UserComponent` to insert a new `Picture` when needed:

@@snip [UserComponent.scala](samples/slick-multidb/src/main/scala/cake/UserComponent.scala) { #insert }

We put all slices of the cake together in `DAL.scala`. The `DAL` class inherits from all components and adds the
missing profile through a field in the constructor:

@@snip [DAL.scala](samples/slick-multidb/src/main/scala/cake/DAL.scala)

This is also a good place to add functionality that affects all components, like the `create` method.
