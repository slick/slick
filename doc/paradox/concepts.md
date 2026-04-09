Core Concepts
==============================

When you use Slick, you start by composing *Scala queries*, then get *actions* (like "get results" or "insert data")
associated with these queries, and finally run the actions on a *database* to obtain *results*.

This chapter explains how these core concepts relate to each other and how they fit into your application design.

Scala Queries
-------------

The main type used by queries is @scaladoc[Rep](slick.lifted.Rep). A `Rep[T]` is a *representation* of a type `T` that
provides the necessary operations for building queries. Collection-valued queries are always of type
@scaladoc[Query](slick.lifted.Query), which is a `Rep` of a collection type like `Rep[Seq[Int]]`. Queries can be composed
from representations of database tables (@scaladoc[TableQuery](slick.lifted.TableQuery)), literal values and parameters. Query
composition does not require a database or execute any part of the query. It only builds a description of what to
execute at a later point.

Database I/O Actions
--------------------

Operations that can be executed on a database are called *database I/O actions* (@scaladoc[DBIOAction](slick.dbio.DBIOAction)).
Several operations on *queries* and *tables* create I/O actions, for example `myQuery.result`,
`myQuery.result.headOption`, `myQuery += data` or `myTable.schema.create`. Actions can be composed with
combinators like `andThen`, `flatMap`, `DBIO.seq` or `transactionally`.

Just like a query, an I/O action is only a *description* of an operation. Creating or composing actions does not execute
anything on a database. Combined actions always consist of *strictly linear sequences* of other actions. Parts of an
action never run concurrently.

Plain SQL Statements
--------------------

As an alternative to Scala queries you can write queries and other database statements in SQL. This is done with
string interpolators, for example `sql"select id from mytable".as[Int]` or
`sqlu"insert into mytable (id) values (1)"`. These interpolators (in the case of `sql` with an extra `.as` call)
all produce *database I/O actions*.

Databases
---------

A @scaladoc[Database](slick.jdbc.JdbcBackend#Database:Database) object encapsulates the resources that are required to
connect to a specific database. This *can* be just a number of connection parameters but in most cases it includes a
*connection pool*. `Database` is parameterized by an effect type `F[_]` (e.g. `cats.effect.IO`) and is constructed
as a `Resource[F, Database[F]]`. You should create the `Database` resource once and keep it for the lifetime of your
application; the resource finalizer closes the connection pool automatically.

Results
-------

Any *action* can be run on a database to obtain the results (or perform side effects such as updating the database).
Execution is always asynchronous, i.e. it does not block the caller thread. Any kind of action can be run to obtain
an `F[R]` value (for example `IO[R]`) that completes asynchronously with the result (`myDatabase.run(myAction)`).
Actions that produce a sequence of values also support streaming results. Such an action can be combined with a
database to produce an @extref[FS2](fs2:) `Stream[F, T]` (`myDatabase.stream(myAction)`). The action is not
executed until the stream is consumed.

Profiles
--------

Even when using a standard interface for database drivers like @extref[JDBC](jdbc:) there are many differences between databases in
the SQL dialect they understand, the way they encode data types, or other idiosyncracies. Slick abstracts over these
differences with *profiles*. Whenever you write queries (whether in Scala or SQL) or produce other database actions,
you need a concrete profile for your database. Usually these profiles extend the abstract @scaladoc[JdbcProfile](slick.jdbc.JdbcProfile).
`Database` objects are interchangeable between all subtypes of JdbcProfile but they are usually configured together
with the profile because you need to pick the correct profile for the database.
