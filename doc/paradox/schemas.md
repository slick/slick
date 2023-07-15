Schemas
=======

This chapter describes how to work with database schemas in Scala code, in particular how to write
them manually, which is useful when you start writing an application without a pre-existing database.
If you already have a schema in the database, you can also use the  @ref:[code generator](code-generation.md)
to take this work off your hands.

@@@ note
In the code examples below we assume the following imports:

@@snip [LiftedEmbedding.scala](../code/LiftedEmbedding.scala) { #imports }

If you're new to Slick, please start with the  @ref:[Getting Started](gettingstarted.md) page.
@@@

Table Rows
----------

In order to use the Scala API for type-safe queries, you need to
define `Table` row classes for your database schema. These describe the
structure of the tables:

@@snip [LiftedEmbedding.scala](../code/LiftedEmbedding.scala) { #tabledef }

All columns are defined through the `column` method. Each column has a
Scala type and a column name for the database (usually in upper-case). The
following primitive types are supported out of the box for JDBC-based
databases in `JdbcProfile` (with certain limitations imposed by the
individual database profiles):

- *Numeric types*: Byte, Short, Int, Long, BigDecimal, Float, Double
- *LOB types*: java.sql.Blob, java.sql.Clob, Array[Byte]
- *Date types*: java.sql.Date, java.sql.Time, java.sql.Timestamp
- Java 8 date and time types: java.time.*
- Boolean
- String
- Unit
- java.util.UUID

Nullable columns are represented by `Option[T]` where `T` is one of the
supported primitive types.

@@@ note
Note: Currently all operations on Option values use the database's null propagation semantics
which may differ from Scala's Option semantics. In particular, `None === None` evaluates
to `None`. This behaviour may change in a future major release of Slick.
@@@

After the column name, you can add optional column options to a `column`
definition. The applicable options are available through the table's `O`
object. The following ones are defined for `JdbcProfile`:

- `PrimaryKey`:  Mark the column as a (non-compound) primary key when creating the DDL statements.

- `Default[T](defaultValue: T)`:  Specify a default value for inserting data into the table without this column.
  This information is only used for creating DDL statements so that the database can fill in the missing information.

- `Unique`: Add a uniqueness constraint to the DDL statement for the column.

- `SqlType(typeName: String)`: Use a non-standard database-specific type for the DDL statements (e.g.
  `SqlType("VARCHAR(20)")` for a `String` column).

- `AutoInc`: Mark the column as an auto-incrementing key when creating the DDL statements. Unlike the other column
  options, this one also has a meaning outside of DDL creation: Many databases do not allow non-AutoInc columns to
  be returned when inserting data (often silently ignoring other columns), so Slick will check if the return column is
  properly marked as AutoInc where needed.

Every table requires a `*` method containing a default projection.
This describes what you get back when you return rows (in the form of a
table row object) from a query. Slick's `*` projection does not have to match
the one in the database. You can add new columns (e.g. with computed values)
or omit some columns as you like. The non-lifted type corresponding to the
`*` projection is given as a type parameter to `Table`. For simple,
non-mapped tables, this will be a single column type or a tuple of column
types.

If your database layout requires *schema names*, you can specify the schema
name for a table in front of the table name, wrapped in `Some()`:

@@snip [LiftedEmbedding.scala](../code/LiftedEmbedding.scala) { #schemaname } 
@ref:[See notes here on the java.time.* types](datetimetypes.md)

Table Query
-----------

Alongside the `Table` row class you also need a `TableQuery` value
which represents the actual database table:

@@snip [LiftedEmbedding.scala](../code/LiftedEmbedding.scala) { #tablequery }

The simple `TableQuery[T]` syntax is a
macro which expands to a proper TableQuery instance that calls the table's
constructor (`new TableQuery(new T(_))`).

You can also extend `TableQuery` to use it as a convenient namespace for
additional functionality associated with the table:

@@snip [LiftedEmbedding.scala](../code/LiftedEmbedding.scala) { #tablequery2 }

Mapped Tables
-------------

It is possible to define a mapped table that uses a custom type for its `*`
projection by adding a bi-directional mapping to the custom type.

Mapped projections can also be created from individual columns, tuples of
columns, or existing mapped projections.

### Case classes with the `mapTo` macro

It is possible to use the convenience method `mapTo` in most circumstances,
which uses a compile-time macro to automatically fill in mapping functions.

@@snip [LiftedEmbedding.scala](../code/LiftedEmbedding.scala) { #maptotable }

The macro should work for most case classes, even those with user-defined companion
objects. However there are still come cases where you will have to use the
`<>` operator, as described below.

### Arbitrary mapping functions with the `<>` operator

You can also map to arbitrary types by using the `<>` operator. It takes
two functions as parameters, one for mapping from the raw value (which may
be a tuple) to the custom type, and one for mapping back.

The API is designed to be optimal for Scala 2 case classes, which generate
a companion object with a `tupled` method, and an `unapply` method that
returns an `Option` of the case class.

When using it with arbitrary mapping functions, however, it can be useful
to call `.shaped` on the column or columns tuple, to get its
type inferred properly. Otherwise you may have to add full type annotations
to the mapping functions.

@@snip [LiftedEmbedding.scala](../code/LiftedEmbedding.scala) { #mappedtable }

@@@ note {title="No tupled method?"}

In Scala 2, case classes generate a default companion object that extends a function type. For instance, in the above
snippet the compiler generates `object User extends ((Option[Int], String, String) => User)`. Since scala function types
contain a `tupled` method, in the above snippet you could replace `(User.apply _).tupled` with just `User.tupled`.
However, this is not a given.

  * In Scala 2, if you explicitly define a companion object, it will not automatically extend a function type.
  * In Scala 3, companions never extend a function type.

The workaround is to lift the `apply` method into a function type, and call `tupled` on that. That is why in the above
snippet we use `(User.apply _).tupled` instead of `User.tupled`.

Alternatively, you can manually define `.tupled` in the companion object:

```scala
object User {
  def tupled = (apply _).tupled
}
```

@@@

Constraints
-----------

A foreign key constraint can be defined with a Table's
@scaladoc[foreignKey](slick.relational.RelationalTableComponent$Table#foreignKey[P,PU,TT%3C:AbstractTable[_],U](String,P,TableQuery[TT])((TT)=%3EP,ForeignKeyAction,ForeignKeyAction)(Shape[_%3C:FlatShapeLevel,TT,U,_],Shape[_%3C:FlatShapeLevel,P,PU,_]):ForeignKeyQuery[TT,U])
method. It first takes a name for the constraint, the referencing column(s) and the referenced table. The second
argument list takes a function from the referenced table to its referenced column(s) as well as
@scaladoc[ForeignKeyAction](slick.model.ForeignKeyAction$) for `onUpdate` and `onDelete`, which are optional and default
to @scaladoc[NoAction](slick.model.ForeignKeyAction$$NoAction$). When creating the DDL statements for the table, the
foreign key definition is added to it.

@@snip [LiftedEmbedding.scala](../code/LiftedEmbedding.scala) { #foreignkey }

Independent of the actual constraint defined in the database, such a foreign
key can be used to navigate to the referenced data with a *join*. For this
purpose, it behaves the same as a manually defined utility method for finding
the joined data:

@@snip [LiftedEmbedding.scala](../code/LiftedEmbedding.scala) { #foreignkeynav }

A primary key constraint can be defined in a similar fashion by adding a
method that calls `primaryKey`. This is useful for defining compound
primary keys (which cannot be done with the `O.PrimaryKey` column option):

@@snip [LiftedEmbedding.scala](../code/LiftedEmbedding.scala) { #primarykey }

Other indexes are defined in a similar way with the `index` method. They
are non-unique by default unless you set the `unique` parameter:

@@snip [LiftedEmbedding.scala](../code/LiftedEmbedding.scala) { #index }

All constraints are discovered reflectively by searching for methods with
the appropriate return types which are defined in the table. This behavior
can be customized by overriding the `tableConstraints` method.

Data Definition Language
------------------------

DDL statements for a table can be created with its `TableQuery`'s `schema` method. Multiple
`DDL` objects can be concatenated with `++` to get a compound `DDL` object which can create
and drop all entities in the correct order, even in the presence of cyclic dependencies between
tables. The `create`, `createIfNotExists`, `dropIfExists`, `drop` and `truncate` methods produce the Actions for executing the DDL statements. To safely create and drop tables use the methods `createIfNotExists` and `dropIfExists` :

@@snip [LiftedEmbedding.scala](../code/LiftedEmbedding.scala) { #ddl }

You can use the the `statements` method to get the SQL code, like for most other SQL-based
Actions. Schema Actions are currently the only Actions that can produce more than one statement.

@@snip [LiftedEmbedding.scala](../code/LiftedEmbedding.scala) { #ddl2 }


@@@ index
  * @ref:[.](datetimetypes.md)
@@@
