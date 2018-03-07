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

On top of the configuration syntax for `Database`, there is another layer in the form of
<api:slick.basic.DatabaseConfig> which allows you to configure a Slick profile plus a
matching `Database` together. This makes it easy to abstract over different kinds of
database systems by simply changing a configuration file.

Here is a typical `DatabaseConfig` with a Slick profile object in `profile` and the database
configuration in `db`:

```yaml src=../code/application.conf#tsql
```
