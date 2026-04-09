# Introduction

## What is Slick?

Slick ("Scala Language-Integrated Connection Kit") is @extref[Lightbend](lightbend:)'s Functional Relational Mapping
(FRM) library for Scala that makes it easy to work with relational databases. It allows you to work with stored data
almost as if you were using Scala collections while at the same time giving you full control over when database access
happens and which data is transferred. You can also use SQL directly. Execution of database actions is done
asynchronously, making Slick a perfect fit for applications built with
@extref[Cats Effect](cats-effect:) or any other CE3-compatible effect system.

@@snip [GettingStartedOverview.scala](../code/GettingStartedOverview.scala){#what-is-slick-micro-example}


When using Scala instead of raw SQL for your queries you benefit from compile-time safety
and compositionality. Slick can generate queries for different back-end databases including
your own, using its extensible query compiler.

See  @ref:[here](supported-databases.md) for an overview of the supported database systems for which
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

@@snip [GettingStartedOverview.scala](../code/GettingStartedOverview.scala) { #quick-schema }

Slick integrates databases directly into Scala, allowing stored and remote data to be queried and
processed in the same way as in-memory data, using ordinary Scala classes and collections.

@@snip [GettingStartedOverview.scala](../code/GettingStartedOverview.scala) { #features-scala-collections }

This enables full control over when a database is accessed and which data is transferred. The
language integrated query model in Slick's FRM is inspired by the LINQ project at Microsoft
and leverages concepts tracing all the way back to the early work of Mnesia at Ericsson.

Some of the key benefits of Slick's FRM approach for functional programming include:

* Efficiency with Pre-Optimization

    FRM is a more efficient way to connect; unlike ORM it has the ability to pre-optimize its
  communication with the database - and with FRM you get this out of the box. The road to making an
  app faster is much shorter with FRM than ORM.

* No More Tedious Troubleshooting with Type Safety<br/>
  
    FRM brings type safety to building database queries. Developers are more productive because the
    compiler finds errors automatically versus the typical tedious troubleshooting required of finding
    errors in untyped strings.

    @@snip [GettingStartedOverview.scala](../code/GettingStartedOverview.scala) { #features-type-safe }

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

    @@snip [GettingStartedOverview.scala](../code/GettingStartedOverview.scala) { #features-composable }

## Effectful Applications

Slick is built for asynchronous, non-blocking application designs. It integrates natively with
@extref[Cats Effect 3](cats-effect:) and @extref[FS2](fs2:):

* **`F[_]: Async` integration**: `db.run(action)` returns `F[R]` — for example `IO[R]` — which
  composes directly in any CE3 program without any conversion or bridging.

* **Clean separation of I/O and computation**: JDBC calls execute on CE3's built-in blocking
  thread pool via `F.blocking`. Your main compute pool stays free for CPU-bound work.

* **FS2 streaming with native back-pressure**: `db.stream(action)` returns `Stream[F, T]`. Back-
  pressure is structural — when the consumer is slow, the fiber suspends and the OS thread is
  released. No Reactive Streams subscription protocol is needed for the common case.

* **Correct resource lifecycle**: `Database[F]` is constructed as a `Resource[F, Database[F]]`.
  Shutting down the connection pool happens automatically when the `Resource` scope ends, even on
  error or fiber cancellation. No manual `close()` call is needed.

* **Transaction rollback on cancellation**: when a fiber running a `.transactionally` action is
  cancelled, the transaction is rolled back. This guarantee was not achievable with
  `Future`-based libraries.

* **Effect-system agnostic**: the `Async[F]` constraint means Slick 4 works with any CE3-
  compatible effect type — `cats.effect.IO`, ZIO via `zio-interop-cats`, Monix `Task`, and
  others — without any special per-effect-system support.

## Plain SQL Support

The Scala-based query API for Slick allows you to write database queries like queries for
Scala collections. Please see  @ref:[Getting Started](gettingstarted.md) for an introduction. Most of this
user manual focuses on this API.

If you want to write your own SQL statements and still execute them asynchronously like
normal Slick queries, you can use the  @ref:[Plain SQL](sql.md) API:

@@snip [GettingStartedOverview.scala](../code/GettingStartedOverview.scala) { #what-is-slick-micro-example-plainsql }

## License

Slick is released under a BSD-Style free and open source software @github[license](/LICENSE.txt).

## Next Steps

* If you are new to Slick, continue to  @ref:[Getting Started](gettingstarted.md)
* If you are migrating from Slick 3, read the  @ref:[Migrating to Slick 4](migrating-to-slick4.md) guide
* If you have used an ORM before, see  @ref:[Coming from ORM to Slick](orm-to-slick.md)
* If you are familiar with SQL, see  @ref:[Coming from SQL to Slick](sql-to-slick.md)
