# Introduction

What is Slick?
--------------

Slick ("Scala Language-Integrated Connection Kit") is @extref[Lightbend](lightbend:)'s Functional Relational Mapping
(FRM) library for Scala that makes it easy to work with relational databases. It allows you to work with stored data
almost as if you were using Scala collections while at the same time giving you full control over when database access
happens and which data is transferred. You can also use SQL directly. Execution of database actions is done
asynchronously, and Slick provides API facades for Cats Effect/FS2, Scala Future/Reactive Streams,
and ZIO/ZStream.

@@snip [GettingStartedOverview.scala](../code/GettingStartedOverview.scala){#what-is-slick-micro-example}


When using Scala instead of raw SQL for your queries you benefit from compile-time safety
and compositionality. Slick can generate queries for different back-end databases including
your own, using its extensible query compiler.

See  @ref:[here](supported-databases.md) for an overview of the supported database systems for which
Slick can generate code.

Why Slick?
----------

Slick gives you a middle path between writing raw SQL strings everywhere and using a classic ORM:

* **Type-safe, composable queries** built with normal Scala expressions.
* **Explicit execution model**: query construction and query execution are separate.
* **Asynchronous execution** with materialized (`run`) and streaming (`stream`) modes.
* **Direct SQL support** when writing SQL manually is the right tool.

@@snip [GettingStartedOverview.scala](../code/GettingStartedOverview.scala) { #quick-schema }

@@snip [GettingStartedOverview.scala](../code/GettingStartedOverview.scala) { #features-scala-collections }

The same type safety applies to mistakes that would otherwise become runtime SQL errors:

@@snip [GettingStartedOverview.scala](../code/GettingStartedOverview.scala) { #features-type-safe }

And queries stay composable as your codebase grows:

@@snip [GettingStartedOverview.scala](../code/GettingStartedOverview.scala) { #features-composable }

Effectful Applications
----------------------

Slick is built for asynchronous, non-blocking application designs.

Slick provides three API facades depending on your effect system:

* **Cats Effect / FS2 (`slick.cats.Database`)** in the core `slick` module
* **Scala Future / Reactive Streams (`slick.future.Database`)** in `slick-future`
* **ZIO / ZStream (`slick.zio.Database`)** in `slick-zio`

All three provide the same core operations:

* `db.run(action)` for materialized results
* `db.stream(action)` for streaming results

See @ref:[Database](database.md) for database configuration, dependencies, lifecycle
management, and complete examples for all three facades.

Plain SQL Support
-----------------

The Scala-based query API for Slick allows you to write database queries like queries for
Scala collections. Please see  @ref:[Getting Started](gettingstarted.md) for an introduction. Most of this
user manual focuses on this API.

If you want to write your own SQL statements and still execute them asynchronously like
normal Slick queries, you can use the  @ref:[Plain SQL](sql.md) API:

@@snip [GettingStartedOverview.scala](../code/GettingStartedOverview.scala) { #what-is-slick-micro-example-plainsql }

License
-------

Slick is released under a BSD-Style free and open source software @github[license](/LICENSE.txt).

Next Steps
----------

* If you are new to Slick, continue to  @ref:[Getting Started](gettingstarted.md)
* For database setup and lifecycle management, see @ref:[Database](database.md)
* If you are migrating from Slick 3, read the  @ref:[Migrating to Slick 4](migrating-to-slick4.md) guide
* If you have used an ORM before, see  @ref:[Coming from ORM to Slick](orm-to-slick.md)
* If you are familiar with SQL, see  @ref:[Coming from SQL to Slick](sql-to-slick.md)
