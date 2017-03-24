# Getting Started {index="sample;download"}

The easiest way to get started is with a working sample application. The following samples are part of the official
Slick distribution. You can either clone Slick from github or download pre-packaged zip files with an indiviual
sample plus an [sbt] launcher.

* To learn the basics of Slick, start with the **Hello Slick** sample ([github](samplerepo:hello-slick),
  [zip](samplezip:hello-slick)). This is the one we are using in this chapter.

* The **Plain SQL Queries** sample ([github](samplerepo:slick-plainsql), [zip](samplezip:slick-plainsql)) shows you how
  to do SQL queries with Slick. See [](sql.md) for details.

* The **Multi-DB Patterns** sample ([github](samplerepo:slick-multidb), [zip](samplezip:slick-multidb)) shows you how
  to write Slick applications that can use different database systems and how to use custom database functions in
  Slick queries.

* The **TestKit** sample ([github](samplerepo:slick-testkit-example), [zip](samplezip:slick-testkit-example)) shows you
  how to use Slick TestKit to test your own database profiles.

## Hello Slick

The *Hello Slick* sample contains simple Scala application, `HelloSlick.scala`, that does basic FRM operations with
Slick. You can run it out of the box with `sbt run`. To make things simple this project uses an embedded in-memory
[H2] database, so no database installation or configuration is required.

The file `TableSuite.scala` contains ScalaTest tests which perform some basic integration tests. You can run these
tests with `sbt test`.

> {.note}
> Note: The example code in this app has intentionally verbose type information. In normal applications type inference
> is used more extensively but to assist with learning the type information was included. 

## Adding Slick to Your Project {index="Maven; sbt; artifacts; build; dependency; logging; SLF4j"}

To include Slick in an existing project use the library published on Maven Central. Add the following to your
build definition (`build.sbt` for [sbt] or `pom.xml` for Maven):

```scala expandVars=true tab=sbt
libraryDependencies ++= Seq(
  "com.typesafe.slick" %% "slick" % "{{version}}",
  "org.slf4j" % "slf4j-nop" % "1.6.4",
  "com.typesafe.slick" %% "slick-hikaricp" % "{{version}}"
)
```

```xml expandVars=true tab=Maven
<!-- Make sure to use the correct Scala version suffix "_2.11" or "_2.12"
     to match your project's Scala version. -->
<dependencies>
  <dependency>
    <groupId>com.typesafe.slick</groupId>
    <artifactId>slick_2.11</artifactId>
    <version>{{version}}</version>
  </dependency>
  <dependency>
    <groupId>org.slf4j</groupId>
    <artifactId>slf4j-nop</artifactId>
    <version>1.6.4</version>
  </dependency>
  <dependency>
    <groupId>com.typesafe.slick</groupId>
    <artifactId>slick-hikaricp_2.11</artifactId>
    <version>{{version}}</version>
  </dependency>
</dependencies>
```

Slick uses [SLF4J] for its own debug logging so you also need to add an SLF4J
implementation. *Hello Slick* uses `slf4j-nop` to disable logging. You have
to replace this with a real logging framework like [Logback] if you want to see
log output.

The [Reactive Streams] API is pulled in automatically as a transitive dependency.

If you want to use Slick's connection pool support for [HikariCP], you need to add
the `slick-hikaricp` module as a dependency as shown above. It will automatically
provide a compatible version of HikariCP as a transitive dependency. Otherwise, you
might need to disable connection pooling or specify a third-party connection pool.

## Quick Introduction

To use Slick you first need to import the API for the database you will be using, like:

```scala src=../code/FirstExample.scala#imports
```

Since we are using [H2] as our database system, we need to import features
from Slick's `H2Profile`. A profile's `api` object contains all commonly
needed imports from the profile and other parts of Slick such as
[database handling](database.md).

Slick's API is fully asynchronous and runs database calls in a separate thread pool. For running
user code in composition of `DBIOAction` and `Future` values, we import the global
`ExecutionContext`. When using Slick as part of a larger application (e.g. with [Play] or
[Akka]) the framework may provide a better alternative to this default `ExecutionContext`.

### Database Configuration

In the body of the application we create a `Database` object which specifies how to connect to a
database. In most cases you will want to configure database connections with [Typesafe Config] in
your `application.conf`, which is also used by [Play] and [Akka] for their configuration:

```yaml src=../code/application.conf#h2mem1
```

For the purpose of this example we disable the connection pool (there is no point in using one for
an embedded in-memory database). When you use a real, external database server, the connection pool provides
improved performance and resilience.

The `keepAliveConnection` option (which is only available without a connection pool) keeps an extra connection
open for the lifetime of the `Database` object in the application. This ensures that the
database does not get dropped while we are using it.

*Hello Slick* is a standalone command-line application, not running inside of a container which takes care of
resource management, so we have to do it ourselves. Since all database calls in Slick are asynchronous, we are
going to compose Futures throughout the app, but eventually we have to wait for the result. This gives us the
following scaffolding:

```scala src=../code/FirstExample.scala#setup
```

> {.note}
> Note: A `Database` object usually manages a thread pool and a connection pool. You should always
> shut it down properly when it is no longer needed (unless the JVM process terminates anyway).
> Do not create a new `Database` for every database operation. A single instance is meant to be kept
> alive for the entire lifetime your your application.

If you are not familiar with asynchronous, Future-based programming Scala, you can learn more about
[Futures and Promises](http://docs.scala-lang.org/overviews/core/futures.html) in the Scala documentation.

### Schema

Before we can write Slick queries, we need to describe a database schema with `Table` row classes
and `TableQuery` values for our tables. You can either use the [code generator](code-generation.md)
to automatically create them for your database schema or you can write them by hand:

```scala src=../code/FirstExample.scala#tables
```

All columns get a name (usually in camel case for Scala and upper case with underscores for SQL) and a
Scala type (from which the SQL type can be derived automatically). The table object also needs a Scala
name, SQL name and type. The type argument of the table must match the type of the special `*` projection.
In simple cases this is a tuple of all columns but more complex mappings are possible.

The `foreignKey` definition in the `coffees` table ensures that the `supID` field can only contain values
for which a corresponding `id` exists in the `suppliers` table, thus creating an *n to one* relationship:
A `Coffees` row points to exactly one `Suppliers` row but any number of coffees can point to the same
supplier. This constraint is enforced at the database level.

### Populating the Database

The connection to the embedded H2 database engine provides us with an empty database. Before we can
execute queries, we need to create the database schema (consisting of the `coffees` and `suppliers` tables)
and insert some test data:

```scala src=../code/FirstExample.scala#create
```

The `TableQuery`'s `schema` method creates `DDL` (data definition language) objects with the database-specific
code for creating and dropping tables and other database entities. Multiple `DDL` values can be combined with
`++` to allow all entities to be created and dropped in the correct order, even when they have circular
dependencies on each other.

Inserting the tuples of data is done with the `+=` and `++=` methods, similar to how you add data to mutable
Scala collections.

The `create`, `+=` and `++=` methods return *database I/O actions* (`DBIOAction`) which can be executed on a database
at a later time to produce a result. If you do not care about more advanced features like streaming, effect tracking
or extension methods for certain actions, you can denote their type as `DBIO[T]` (for an operation which will
eventually produce a value of type `T`).

There are several different combinators for combining multiple `DBIOAction`s into sequences, yielding another action.
Here we use the simplest one, `DBIO.seq`, which can concatenate any number of actions, discarding the return values
(i.e. the resulting `DBIOAction` produces a result of type `Unit`). We then execute the setup action asynchronously
with `db.run`, yielding a `Future[Unit]`.

> {.note}
> Note: Database connections and transactions are managed automatically by Slick. By default
> connections are acquired and released on demand and used in *auto-commit* mode. In this mode we
> have to populate the `suppliers` table first because the `coffees` data can only refer to valid
> supplier IDs. We could also use an explicit transaction bracket encompassing all these statements
> (`db.run(setup.transactionally)`). Then the order would not matter because the constraints are
> only enforced at the end when the transaction is committed.

When inserting data, the database usually returns the number of affected rows, therefore the return type is
`Option[Int]` as can be seen in this definition of `insertAction`:

```scala src=../../samples/hello-slick/src/main/scala/HelloSlick.scala#insertAction
```

We can use the `map` combinator to run some code and compute a new value from the value returned by the action
(or in this case run it only for its side effects and return `Unit`).

> {.note}
> Note that `map` and all other combinators which run user code (e.g. `flatMap`, `cleanup`, `filter`) take an implicit
> `ExecutionContext` on which to run this code. Slick uses its own `ExecutionContext` internally for running blocking
> database I/O but it always maintains a clean separation and prevents you from running non-I/O code on it.

### Querying

The simplest kind of query iterates over all the data in a table by calling `.result` on the `TableQuery` to get
a `DBIOAction`:

```scala src=../code/FirstExample.scala#readall
```

This corresponds to a `SELECT * FROM COFFEES` in SQL (except that the `*` is the table's `*` projection
we defined earlier and not whatever the database sees as `*`). The type of the values we get in the loop
is, unsurprisingly, the type parameter of `Coffees`.

Let's add a *projection* to this basic query. This is written in Scala with the `map` method or a
*for comprehension*:

```scala src=../code/FirstExample.scala#projection
```

The output will be the same: for each row of the table, all columns get converted to strings and concatenated
into one tab-separated string. The difference is that all of this now happens inside the database engine, and
only the resulting concatenated string is shipped to the client. Note that we avoid Scala's `+` operator
(which is already heavily overloaded) in favor of `++` (commonly used for sequence concatenation). Also,
there is no automatic conversion of other argument types to strings. This has to be done explicitly with the
type conversion method `asColumnOf`.

This time we also use [Reactive Streams] to get a streaming result from the database and print the elements
as they come in instead of materializing the whole result set upfront.

Joining and filtering tables is done the same way as when working with Scala collections:

```scala src=../code/FirstExample.scala#join
```

> {.warning}
> Note the use of `===` instead of `==` for comparing two values for equality and `=!=`
> instead of `!=` for inequality. This is necessary because these operators are already defined
> (with unsuitable types and semantics) on the base type `Any`, so they cannot be replaced by
> extension methods. The other comparison operators are the same as in standard Scala code:
> `<`, `<=`, `>=`, `>`.

The generator expression `suppliers if s.id === c.supID` follows the relationship established by the foreign
key `Coffees.supplier`. Instead of repeating the join condition here we can use the foreign key directly:

```scala src=../code/FirstExample.scala#fkjoin
```

### Aggregations

Aggregates values like minimum, maximum, summation, and average can be computed by the database using the query
functions `min`, `max`, `sum` and `avg` like:

```scala src=../../samples/hello-slick/src/main/scala/HelloSlick.scala#maxPrice
```

This creates a new scalar query (`Rep`) that can be run like a collection-valued `Query` by calling `.result`.

### Plain SQL / String Interpolation

Sometimes writing SQL code manually is the easiest and best way to go but we don't want to lose SQL injection
protection that Slick includes. [SQL String Interpolation](sql.md) provides a nice API for doing this.
In *Hello Slick* we use the `sql` interpolator:

```scala src=../../samples/hello-slick/src/main/scala/HelloSlick.scala#plainSql
```

This produces a database I/O action that can be run or streamed in the usual way.

### Case Class Mapping

The `CaseClassMapping.scala` app provides an example which uses a *case class* instead of tupled values.
To use case classes instead of tuples setup a `def *` projection which transforms the tuple values to and from the
case class. For example:

```scala src=../../samples/hello-slick/src/main/scala/CaseClassMapping.scala#mapTo
```

This uses the `mapTo` macro to convert between `(Option[Int], String)` and `User` bidirectionally. Now all of the
queries can work with a `User` object instead of the tuples. 

See [](schemas.md#mapped-tables) for details.

### Auto-Generated Primary Keys

The `Users` table mapping in `CaseClassMapping.scala` defines an `id` column which uses an auto-incrementing
primary key:

```scala src=../../samples/hello-slick/src/main/scala/CaseClassMapping.scala#autoInc
```

See [](schemas.md#table-rows) for more column options.

### Running Queries

So far you have seen how to get a `Seq` from a collection-valued query and how to stream individual elements.
There are several other useful methods which are shown in `QueryActions.scala`. They are equally applicable to
[Scala queries](queries.md) and [Plain SQL queries](sql.md).

Note the use of `Compiled` in this app. It is used to define a pre-compiled query that can be executed with
different parameters without having to recompile the SQL statement each time. This is the preferred way of defining
queries in real-world applications. It prevents the (possibly expensive) compilation each time and leads to the
same SQL statement (or a small, fixed set of SQL statements) so that the database system can also reuse a previously
computed execution plan. As a side-effect, all parameters are automatically turned into bind variables:

```scala src=../../samples/hello-slick/src/main/scala/QueryActions.scala#upTo
```

See [](queries.md#compiled-queries) for details.
