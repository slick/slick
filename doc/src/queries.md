Queries {index="lifted; Query"}
=======

This chapter describes how to write type-safe queries for selecting, inserting, updating and
deleting data with Slick's Scala-based query API.

 > {.note}
 > In the code examples below we assume the following imports:
 >```scala src=../code/LiftedEmbedding.scala#imports
 > ```
 > If you're new to Slick, please start with the [Getting Started](gettingstarted.md) page.

The API for building queries is a
*lifted embedding*, which means that you are not working with standard Scala types but with types
that are *lifted* into a <api:slick.lifted.Rep> type constructor. This becomes clearer when you
compare the types of a simple Scala collections example

```scala src=../code/LiftedEmbedding.scala#plaintypes
```

... with the types of similar code in Slick:

```scala src=../code/LiftedEmbedding.scala#reptypes
```

All plain types are lifted into `Rep`. The same is true for the table row
type `Coffees` which is a subtype of `Rep[(String, Double)]`.
Even the literal `8.0` is automatically lifted to a `Rep[Double]` by an
implicit conversion because that is what the `>` operator on
`Rep[Double]` expects for the right-hand side. This lifting is necessary
because the lifted types allow us to generate a syntax tree that captures
the query computations. Getting plain Scala functions and values would not
give us enough information for translating those computations to SQL.

Expressions {index="expression; scalar; collection-valued; Rep; Seq; extension method; select; projection; view"}
-----------

Scalar (non-record, non-collection) values are represented by type `Rep[T]` for which an implicit
`TypedType[T]` exists.

The operators and other methods which are commonly used in queries
are added through implicit conversions defined in
`ExtensionMethodConversions`. The actual methods can be found in
the classes `AnyExtensionMethods`, `ColumnExtensionMethods`,
`NumericColumnExtensionMethods`, `BooleanColumnExtensionMethods` and
`StringColumnExtensionMethods`
(cf. [ExtensionMethods](slick:slick/src/main/scala/slick/lifted/ExtensionMethods.scala)).

> {.warning}
> Warning: Most operators mimic the plain Scala equivalents, but you have to use `===` instead of
> `==` for comparing two values for equality and `=!=` instead of `!=` for inequality.
> This is necessary because these operators are already defined (with unsuitable types and
> semantics) on the base type `Any`, so they cannot be replaced by extension methods.
> Similarly, to combine a Scala `String` with a `Rep[String]` or `ConstColumn[String]`, 
> don't use `str + rep` syntax, since Scala provides a universal `+` operator for strings
> that works with `Any`.  Instead use slick's `++` operator: `(str: Rep[String]) ++ rep`

Collection values are represented by the `Query` class (a `Rep[Seq[T]]`) which contains many
standard collection methods like `flatMap`, `filter`, `take` and `groupBy`. Due to the two
different component types of a `Query` (lifted and plain, e.g. `Query[(Rep[Int], Rep[String]),
(Int, String), Seq]`), the signatures for these methods are very complex but the semantics are
essentially the same as for Scala collections.

Additional methods for queries of scalar values are added via an
implicit conversion to `SingleColumnQueryExtensionMethods`.

Sorting and Filtering {index="sort; filter; order by; sortBy; where"}
---------------------

There are various methods with sorting/filtering semantics (i.e. they take a
`Query` and return a new `Query` of the same type), for example:

```scala src=../code/LiftedEmbedding.scala#filtering
```

Joining and Zipping {index="join; zip; zipWithIndex"}
-------------------

Joins are used to combine two different tables or queries into a single query.
There are two different ways of writing joins: *Applicative* and *monadic*.

### Applicative joins {index="join,outer; outer,join; join,applicative; applicative,join"}

*Applicative* joins are performed by calling a method that joins two queries into a single query
of a tuple of the individual results. They have the same restrictions as joins in SQL, i.e. the
right-hand side may not depend on the left-hand side. This is enforced naturally through Scala's
scoping rules.

```scala src=../code/JoinsUnions.scala#explicit
```

Note the use of `map` in the `yield` clauses of the outer joins. Since these joins can
introduce additional NULL values (on the right-hand side for a left outer join, on the left-hand
sides for a right outer join, and on both sides for a full outer join), the respective sides of
the join are wrapped in an `Option` (with `None` representing a row that was not matched).

### Monadic joins {index="join,monadic; monadic,join; join,inner; inner,join; join,cross; cross,join; join,implicit; implicit,join; join,explicit; explicit,join"}

*Monadic* joins are created with `flatMap`. They are theoretically more powerful than
applicative joins because the right-hand side may depend on the left-hand side. However, this is
not possible in standard SQL, so Slick has to compile them down to applicative joins, which is
possible in many useful cases but not in all of them (and there are cases where it is possible in
theory but Slick cannot perform the required transformation yet). If a monadic join cannot be
properly translated, it will fail at runtime.

A *cross-join* is created with a `flatMap` operation on a `Query`
(i.e. by introducing more than one generator in a for-comprehension):

```scala src=../code/JoinsUnions.scala#implicitCross
```

If you add a filter expression, it becomes an *inner join*:

```scala src=../code/JoinsUnions.scala#implicitInner
```

The semantics of these monadic joins are the same as when you are using
`flatMap` on Scala collections.

> {.note}
> Slick generates *implicit* joins in SQL (`select ... from a, b where ...`) for
> where possible, and *explicit* joins (`select ... from a join b on ...`) otherwise.

### Zip joins {index="join,zip; zip,join"}

In addition to the usual applicative join operators supported by relational databases
(which are based off a cross join or outer join), Slick also has *zip joins*
which create a pairwise join of two queries. The semantics are again the same
as for Scala collections, using the `zip` and `zipWith` methods:

```scala src=../code/JoinsUnions.scala#zip
```

A particular kind of zip join is provided by `zipWithIndex`. It zips a query
result with an infinite sequence starting at 0. Such a sequence cannot be
represented by an SQL database and Slick does not currently support it, either.
The resulting zipped query, however, can be represented in SQL with the use of a *row number* function,
so `zipWithIndex` is supported as a primitive operator:

```scala src=../code/JoinsUnions.scala#zipWithIndex
```

Unions {index="union; ++; unionAll"}
------

Two queries can be concatenated with the `++` (or `unionAll`) and `union`
operators if they have compatible types:

```scala src=../code/JoinsUnions.scala#union
```

Unlike `union` which filters out duplicate values, `++` simply concatenates
the results of the individual queries, which is usually more efficient.

Aggregation {index="aggregate; min; max; sum; avg; length; count; exists; group by; groupBy"}
-----------

The simplest form of aggregation consists of computing a primitive value from a
Query that returns a single column, usually with a numeric type, e.g.:

```scala src=../code/LiftedEmbedding.scala#aggregation1
```

Note that these aggregate queries return a scalar result, not a collection.
Some aggregation functions are defined for arbitrary queries (of more than
one column):

```scala src=../code/LiftedEmbedding.scala#aggregation2
```

Grouping is done with the `groupBy` method. It has the same semantics as for
Scala collections:

```scala src=../code/LiftedEmbedding.scala#aggregation3
```

The intermediate query `q` contains nested values of type `Query`.
These would turn into nested collections when executing the query, which is
not supported at the moment. Therefore it is necessary to flatten the nested
queries immediately by aggregating their values (or individual columns)
as done in `q2`.

Querying {index="querying; first; buildColl; selectStatement; query,execute; execute,query; query,run; run,query"}
--------

A Query can be converted into an [Action](api:slick.dbio.DBIOAction) by calling its
`result` method. The Action can then be [executed](dbio.md#executing-database-io-actions) directly in a
streaming or fully materialized way, or composed further with other Actions:

```scala src=../code/LiftedEmbedding.scala#result
```

If you only want a single result value, you can call `head` or `headOption` on the `result` Action.

Deleting {index="delete; deleteStatement"}
--------

Deleting works very similarly to querying. You write a query which selects the
rows to delete and then get an Action by calling the `delete` method on it:

```scala src=../code/LiftedEmbedding.scala#delete1
```

A query for deleting must only use a single table - no joins are allowed (Slick does not yet support
the `USING` keyword for deletes). Any projection is ignored (it always deletes full rows).

If you need to perform a join, you can `filter` based on another `Query`:

```scala src=../code/LiftedEmbedding.scala#delete2
```

Inserting {index="insert; +=; ++=; insertStatement; returning; AutoInc; generated key; into"}
---------

Inserts are done based on a projection of columns from a single table. When you use the table
directly, the insert is performed against its `*` projection. Omitting some of a table's columns
when inserting causes the database to use the default values specified in the table definition, or
a type-specific default in case no explicit default was given. All methods for building insert
Actions are defined in
<api:slick.jdbc.JdbcActionComponent@CountingInsertActionComposer[U]:JdbcProfile.CountingInsertActionComposer[U]> and
<api:slick.jdbc.JdbcActionComponent@ReturningInsertActionComposer[U,RU]:JdbcProfile.ReturningInsertActionComposer[U,RU]>.

```scala src=../code/LiftedEmbedding.scala#insert1
```

When you include an `AutoInc` column in an insert operation, it is silently
ignored, so that the database can generate the proper value.
In this case you usually want to get back the auto-generated primary key
column. By default, `+=` gives you a count of the number of affected
rows (which will usually be 1) and `++=` gives you an accumulated
count in an `Option` (which can be `None` if the database system does not
provide counts for all rows). This can be changed with the `returning`
method where you specify the columns to be returned (as a single value or
tuple from `+=` and a `Seq` of such values from `++=`):

```scala src=../code/LiftedEmbedding.scala#insert3
```

> {.note}
> Many database systems only allow a single column to be returned
> which must be the table's auto-incrementing primary key. If you ask for
> other columns a `SlickException` is thrown at runtime (unless the database
> actually supports it).

You can follow the `returning` method with the `into` method to map
the inserted values and the generated keys (specified in returning) to a desired value.
Here is an example of using this feature to return an object with an updated id:

```scala src=../code/LiftedEmbedding.scala#insert3b
```

> {.note}
> When using the `++=` batch insert operation, Slick makes use of the JDBC batch API.
> The underlying JDBC driver will decide how to transmit the batch (via SQL) to the database server.
> 
> Slick may fall back to generating multiple insert statements for batch operations.
> This will depend on the driver (if it supports batch insert),
> and other circumstances (`returning` keys is generally not supported in batch operations).

Instead of inserting data from the client side you can also insert data
created by a `Query` or a scalar expression that is executed in the
database server:

```scala src=../code/LiftedEmbedding.scala#insert4
```

In these cases, `AutoInc` columns are *not* ignored.

Updating {index="update; updateStatement"}
--------

Updates are performed by writing a query that selects the data to update and
then replacing it with new data. The query must only return raw columns (no
computed values) selected from a single table. The relevant methods for
updating are defined in
[UpdateExtensionMethods](api:slick.jdbc.JdbcActionComponent@UpdateActionExtensionMethodsImpl[T]:JdbcProfile.UpdateActionExtensionMethodsImpl[T]).

```scala src=../code/LiftedEmbedding.scala#update1
```

There is currently no way to use scalar expressions or transformations of
the existing data in the database for updates.

Upserting
---------

Upserting is performed by supplying a row to be either inserted or updated. The
object must contain the table's primary key, since the update portion needs to
be able to find a uniquelly matching row.

```scala src=../code/LiftedEmbedding.scala#insertOrUpdate
```

Compiled Queries {index="prepared; QueryTemplate; parameter; query,compiled; compiler,query; take; drop"}
----------------

Database queries typically depend on some parameters, e.g. an ID for which
you want to retrieve a matching database row. You can write a regular Scala
function to create a parameterized `Query` object each time you need to
execute that query but this will incur the cost of recompiling the query
in Slick (and possibly also on the database if you don't use bind variables
for all parameters). It is more efficient to pre-compile such parameterized
query functions:

```scala src=../code/LiftedEmbedding.scala#compiled1
```

This works for all functions that take parameters consisting only of individual columns or
[records](userdefined.md#record-types) of columns and return a `Query` object or a
scalar query. See the API documentation for <api:slick.lifted.Compiled>
and its subclasses for details on composing compiled queries.

Be aware that `take` and `drop` take `ConstColumn[Long]` parameters. Unlike `Rep[Long]`,
which could be substituted by another value computed by a query, a ConstColumn can only be literal
value or a parameter of a compiled query. This is necessary because the actual value has to be
known by the time the query is prepared for execution by Slick.

```scala src=../code/LiftedEmbedding.scala#compiled2
```

You can use a compiled query for querying, inserting, updating and deleting data. For
backwards-compatibility with Slick 1.0 you can still create a compiled
query by calling `flatMap` on a <api:slick.lifted.Parameters> object.
In many cases this enables you to write a single *for comprehension* for a
compiled query:

```scala src=../code/LiftedEmbedding.scala#template1
```
