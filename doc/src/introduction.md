# Introduction {index=introduction}

## What is Slick?

Slick ("Scala Language-Integrated Connection Kit") is [Lightbend]'s
Functional Relational Mapping (FRM) library for Scala that makes it easy to work with relational
databases. It allows you to work with stored data almost as if you were using Scala collections
while at the same time giving you full control over when database access happens and which data
is transferred. You can also use SQL directly. Execution of database actions is done
asynchronously, making Slick a perfect fit for your reactive applications based on [Play] and [Akka].

```scala src=../code/GettingStartedOverview.scala#what-is-slick-micro-example
```

When using Scala instead of raw SQL for your queries you benefit from compile-time safety
and compositionality. Slick can generate queries for different back-end databases including
your own, using its extensible query compiler.

See [here](supported-databases.md) for an overview of the supported database systems for which
Slick can generate code.

## Functional Relational Mapping

Functional programmers have long suffered Object-Relational and Object-Math impedance mismatches
when connecting to relational databases. Slick's new Functional Relational Mapping (FRM) paradigm
allows mapping to be completed within Scala, with loose-coupling, minimal configuration requirements,
and a number of other major advantages that abstract the complexities away from connecting with
relational databases.

We don't try to fight the relational model, we embrace it through a functional paradigm. Instead of
trying to bridge the gap between the object model and the database model, we've brought the database
model into Scala so developers don't need to write SQL code.

```scala src=../code/GettingStartedOverview.scala#quick-schema
```

Slick integrates databases directly into Scala, allowing stored and remote data to be queried and
processed in the same way as in-memory data, using ordinary Scala classes and collections.

```scala src=../code/GettingStartedOverview.scala#features-scala-collections
```

This enables full control over when a database is accessed and which data is transferred. The
language integrated query model in Slick's FRM is inspired by the LINQ project at Microsoft
and leverages concepts tracing all the way back to the early work of Mnesia at Ericsson.

Some of the key benefits of Slick's FRM approach for functional programming include:

* Efficiency with Pre-Optimization

  FRM is a more efficient way to connect; unlike ORM it has the ability to pre-optimize its
  communication with the database - and with FRM you get this out of the box. The road to making an
  app faster is much shorter with FRM than ORM.

* No More Tedious Troubleshooting with Type Safety

  FRM brings type safety to building database queries. Developers are more productive because the
  compiler finds errors automatically versus the typical tedious troubleshooting required of finding
  errors in untyped strings.

  ```scala src=../code/GettingStartedOverview.scala#features-type-safe
  ```

  Misspelled the column name ``price``? The compiler will tell you:

  ```text
  GettingStartedOverview.scala:89: value prices is not a member of com.typesafe.slick.docs.GettingStartedOverview.Coffees
          coffees.map(_.prices).result
                        ^
  ```

  The same goes for type errors:

  ```text
  GettingStartedOverview.scala:89: type mismatch;
   found   : slick.jdbc.H2Profile.StreamingProfileAction[Seq[String],String,slick.dbio.Effect.Read]
      (which expands to)  slick.sql.FixedSqlStreamingAction[Seq[String],String,slick.dbio.Effect.Read]
   required: slick.dbio.DBIOAction[Seq[Double],slick.dbio.NoStream,Nothing]
          coffees.map(_.name).result
                              ^
  ```

* A More Productive, Composable Model for Building Queries

  FRM supports a composable model for building queries. It's a very natural model to compose pieces
  together to build a query, and then reuse pieces across your code base.

  ```scala src=../code/GettingStartedOverview.scala#features-composable
  ```

## Reactive Applications {index="database,supported; Derby; JavaDB; H2; HSQLDB; HyperSQL; MySQL; PostgreSQL; SQLite"}

Slick is easy to use in asynchronous, non-blocking application designs, and supports building
applications according to the [Reactive Manifesto]. Unlike simple wrappers around traditional,
blocking database APIs, Slick gives you:

* Clean separation of I/O and CPU-intensive code: Isolating I/O allows you to keep your main
  thread pool busy with CPU-intensive parts of the application while waiting for I/O in the
  background.

* Resilience under load: When a database cannot keep up with the load of your application,
  Slick will not create more and more threads (thus making the situation worse) or lock out all
  kinds of I/O. Back-pressure is controlled efficiently through a queue (of configurable size)
  for database I/O actions, allowing a certain number of requests to build up with very little
  resource usage and failing immediately once this limit has been reached.

* [Reactive Streams] for asynchronous streaming.

* Efficient utilization of database resources: Slick can be tuned easily and precisely for the
  parallelism (number of concurrent active jobs) and resource usage (number of currently
  suspended database sessions) of your database server.

## Plain SQL Support

The Scala-based query API for Slick allows you to write database queries like queries for
Scala collections. Please see [Getting Started](gettingstarted.md) for an introduction. Most of this
user manual focuses on this API.

If you want to write your own SQL statements and still execute them asynchronously like
normal Slick queries, you can use the [Plain SQL](sql.md) API:

```scala src=../code/GettingStartedOverview.scala#what-is-slick-micro-example-plainsql
```

## License {index=license}

Slick is released under a BSD-Style free and open source software [license](slick:LICENSE.txt).

## Next Steps

* If you are new to Slick, continue to [Getting Started](gettingstarted.md)
* If you have used an older version of Slick, make sure to read the [Upgrade Guides](upgrade.md)
* If you have used an ORM before, see [Coming from ORM to Slick](orm-to-slick.md)
* If you are familiar with SQL, see [Coming from SQL to Slick](sql-to-slick.md)
