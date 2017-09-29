Plain SQL Queries {index="SQL; Plain SQL"}
=================

Sometimes you may need to write your own SQL code for an operation which is
not well supported at a higher level of abstraction. Instead of falling back
to the low level of [JDBC], you can use Slick's *Plain SQL* queries with a much
nicer Scala-based API.

> {.note}
> This chapter is based on the **Plain SQL Queries** sample ([github](samplerepo:slick-plainsql),
> [zip](samplezip:slick-plainsql)) which provides a ready-to-run app to demonstrate the features.

Scaffolding
-----------

The database connection is opened
[in the usual way](gettingstarted.md#database-configuration). All *Plain SQL* queries result in
a <api:slick.dbio.DBIOAction> that can be composed and run like any other action.

String Interpolation {index="interpolation; sqlu,interpolator; update,Plain SQL; Plain SQL;update"}
--------------------

*Plain SQL* queries in Slick are built via string interpolation using the `sql`, `sqlu` and
`tsql` interpolators. They are available through the standard `api._` import from a Slick profile:

```scala src=../code/PlainSQL.scala#imports
```

You can see the simplest use case in the following methods where the `sqlu` interpolator is used
with a literal SQL string:

```scala src=../code/PlainSQL.scala#sqlu
```

The `sqlu` interpolator is used for DML statements which produce a row count instead of a result
set. Therefore they are of type `DBIO[Int]`.

Any variable or expression injected into a query gets turned into a bind variable in the resulting
query string. It is not inserted directly into a query string, so there is no danger of SQL
injection attacks. You can see this used in here:

```scala src=../code/PlainSQL.scala#bind
```

The SQL statement produced by this method is always the same:

```sql
insert into coffees values (?, ?, ?, ?, ?)
```

Note the use of the
[DBIO.sequence](api:slick.dbio.DBIOAction$@sequence[R,M[+_]%3C:TraversableOnce[_],E%3C:slick.dbio.Effect](in:M[slick.dbio.DBIOAction[R,slick.dbio.NoStream,E]])(implicitcbf:scala.collection.generic.CanBuildFrom[M[slick.dbio.DBIOAction[R,slick.dbio.NoStream,E]],R,M[R]]):slick.dbio.DBIOAction[M[R],slick.dbio.NoStream,E])
combinator which is useful for this kind of code:

```scala src=../code/PlainSQL.scala#sequence
```

Unlike the simpler
[DBIO.seq](api:slick.dbio.DBIOAction$@seq[E%3C:slick.dbio.Effect](actions:slick.dbio.DBIOAction[_,slick.dbio.NoStream,E]*):slick.dbio.DBIOAction[Unit,slick.dbio.NoStream,E])
combinator which runs a (varargs) sequence of database I/O actions in the given order and discards
the return values,
[DBIO.sequence](api:slick.dbio.DBIOAction$@sequence[R,M[+_]%3C:TraversableOnce[_],E%3C:slick.dbio.Effect](in:M[slick.dbio.DBIOAction[R,slick.dbio.NoStream,E]])(implicitcbf:scala.collection.generic.CanBuildFrom[M[slick.dbio.DBIOAction[R,slick.dbio.NoStream,E]],R,M[R]]):slick.dbio.DBIOAction[M[R],slick.dbio.NoStream,E])
turns a `Seq[DBIO[T]]` into a `DBIO[Seq[T]]`, thus preserving the results of all individual
actions. It is used here to sum up the affected row counts of all inserts.

Result Sets {index="Plain SQL,query; query,Plain SQL; GetResult"}
-----------

The following code uses the `sql` interpolator which returns a result set produced by a
statement. The interpolator by itself does not produce a `DBIO` value. It needs to be
followed by a call to `.as` to define the row type:

```scala src=../code/PlainSQL.scala#sql
```

This results in a `DBIO[Seq[(String, String)]]`. The call to `as` takes an implicit
<api:slick.jdbc.GetResult> parameter which extracts data of the requested type from a result set.
There are predefined `GetResult` implicits for the standard JDBC types, for Options of those (to
represent nullable columns) and for tuples of types which have a `GetResult`. For non-standard
return types you have to define your own converters:

```scala src=../code/PlainSQL.scala#getresult
```

`GetResult[T]` is simply a wrapper for a function `PositionedResult => T`. The implicit val for
`Supplier` uses the explicit `PositionedResult` methods `getInt` and `getString` to read
the next `Int` or `String` value in the current row. The second one uses the shortcut method
`<<` which returns a value of whatever type is expected at this place. (Of course you can only
use it when the type is actually known like in this constructor call.)

Splicing Literal Values
-----------------------

While most parameters should be inserted into SQL statements as bind variables, sometimes you need
to splice literal values directly into the statement, for example to abstract over table names or
to run dynamically generated SQL code. You can use `#$` instead of `$` in all interpolators for
this purpose, as shown in the following piece of code:

```scala src=../code/PlainSQL.scala#literal
```

Type-Checked SQL Statements {index="tsql,interpolator; interpolator,tsql"}
---------------------------

The interpolators you have seen so far only construct a SQL statement at runtime. This provides a
safe and easy way of building statements but they are still just embedded strings. If you have a
syntax error in a statement or the types don't match up between the database and your Scala code,
this cannot be detected at compile-time. You can use the `tsql` interpolator instead of `sql`
to get just that:

```scala src=../code/PlainSQL.scala#tsql
```

Note that `tsql` directly produces a `DBIOAction` of the correct type without requiring a call
to `.as`.

In order to give the compiler access to the database, you have to provide a configuration that can
be resolved at compile-time. This is done with the <api:slick.basic.StaticDatabaseConfig>
annotation:

```scala
@StaticDatabaseConfig("file:src/main/resources/application.conf#tsql")
```

In this case it points to the path "tsql" in a local `application.conf` file, which must contain
an appropriate configuration for a <api:slick.basic.StaticDatabaseConfig> object, not just a
`Database`.

> {.note}
> You can get `application.conf` resolved via the classpath (as usual) by omitting the path and
> only specifying a fragment in the URL, or you can use a `resource:` URL scheme for referencing
> an arbitrary classpath resource, but in both cases, they have to be on the *compiler's* own
> classpath, not just the source path or the runtime classpath. Depending on the build tool this
> may not be possible, so it's usually better to use a relative `file:` URL.

You can also retrieve the statically configured <api:slick.basic.DatabaseConfig> at runtime:

```scala src=../code/PlainSQL.scala#staticdatabaseconfig
```

This gives you the Slick profile for the standard `api._` import and the `Database`. Note that
it is not mandatory to use the same configuration. You can get a Slick profile and `Database` at
runtime in any other way you like and only use the `StaticDatabaseConfig` for compile-time
checking.
