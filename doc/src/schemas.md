Schemas {index=schema}
=======

This chapter describes how to work with database schemas in Scala code, in particular how to write
them manually, which is useful when you start writing an application without a pre-existing database.
If you already have a schema in the database, you can also use the [code generator](code-generation.md)
to take this work off your hands.

> {.note}
> In the code examples below we assume the following imports:
>```scala src=../code/LiftedEmbedding.scala#imports
> ```
> If you're new to Slick, please start with the [Getting Started](gettingstarted.md) page.

Table Rows {index="table; row; ColumnOption; primary key; default; type; AutoInc; generated; identity; *; star projection; schema,name; name,schema"}
----------

In order to use the Scala API for type-safe queries, you need to
define `Table` row classes for your database schema. These describe the
structure of the tables:

```scala src=../code/LiftedEmbedding.scala#tabledef
```

All columns are defined through the `column` method. Each column has a
Scala type and a column name for the database (usually in upper-case). The
following primitive types are supported out of the box for JDBC-based
databases in `JdbcProfile` (with certain limitations imposed by the
individual database profiles):

- *Numeric types*: Byte, Short, Int, Long, BigDecimal, Float, Double
- *LOB types*: java.sql.Blob, java.sql.Clob, Array[Byte]
- *Date types*: java.sql.Date, java.sql.Time, java.sql.Timestamp
- Boolean
- String
- Unit
- java.util.UUID

Nullable columns are represented by `Option[T]` where `T` is one of the
supported primitive types.

> {.note}
> Note: Currently all operations on Option values use the database's null propagation semantics
> which may differ from Scala's Option semantics. In particular, `None === None` evaluates
> to `None`. This behaviour may change in a future major release of Slick.

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

```scala src=../code/LiftedEmbedding.scala#schemaname
```

Table Query {index=TableQuery}
-----------

Alongside the `Table` row class you also need a `TableQuery` value
which represents the actual database table:

```scala src=../code/LiftedEmbedding.scala#tablequery
```

The simple `TableQuery[T]` syntax is a
macro which expands to a proper TableQuery instance that calls the table's
constructor (`new TableQuery(new T(_))`).

You can also extend `TableQuery` to use it as a convenient namespace for
additional functionality associated with the table:

```scala src=../code/LiftedEmbedding.scala#tablequery2
```

Mapped Tables {index="table,mapped; mapped,table; <>; entity; tupled; unapply"}
-------------

It is possible to define a mapped table that uses a custom type for its `*`
projection by adding a bi-directional mapping with the `<>` operator:

```scala src=../code/LiftedEmbedding.scala#mappedtable
```

It is optimized for case classes (with a simple `apply` method and an
`unapply` method that wraps its result in an `Option`) but it can also
be used with arbitrary mapping functions. In these cases it can be useful
to call `.shaped` on a tuple on the left-hand side in order to get its
type inferred properly. Otherwise you may have to add full type annotations
to the mapping functions.

For case classes with hand-written companion objects, `.tupled` only works
if you manually extend the correct Scala function type. Alternatively you can use
`(User.apply _).tupled`. See [](SI:3664) and [](SI:4808).

Constraints {index="constraint; index; key,foreign; foreign,key; key,primary; primary;key"}
-----------

A foreign key constraint can be defined with a Table's
<api:slick.relational.RelationalTableComponent$Table@foreignKey[P,PU,TT%3C:AbstractTable[_],U](String,P,TableQuery[TT])((TT)=%3EP,ForeignKeyAction,ForeignKeyAction)(Shape[_%3C:FlatShapeLevel,TT,U,_],Shape[_%3C:FlatShapeLevel,P,PU,_]):ForeignKeyQuery[TT,U]>
method. It first takes a name for the constraint, the referencing column(s) and the referenced table. The second
argument list takes a function from the referenced table to its referenced column(s) as well as
<api:slick.model.ForeignKeyAction$> for `onUpdate` and `onDelete`, which are optional and default to
<api:slick.model.ForeignKeyAction$$NoAction$>. When creating the DDL statements for the table, the foreign key
definition is added to it.

```scala src=../code/LiftedEmbedding.scala#foreignkey
```

Independent of the actual constraint defined in the database, such a foreign
key can be used to navigate to the referenced data with a *join*. For this
purpose, it behaves the same as a manually defined utility method for finding
the joined data:

```scala src=../code/LiftedEmbedding.scala#foreignkeynav
```

A primary key constraint can be defined in a similar fashion by adding a
method that calls `primaryKey`. This is useful for defining compound
primary keys (which cannot be done with the `O.PrimaryKey` column option):

```scala src=../code/LiftedEmbedding.scala#primarykey
```

Other indexes are defined in a similar way with the `index` method. They
are non-unique by default unless you set the `unique` parameter:

```scala src=../code/LiftedEmbedding.scala#index
```

All constraints are discovered reflectively by searching for methods with
the appropriate return types which are defined in the table. This behavior
can be customized by overriding the `tableConstraints` method.

Data Definition Language {index="DDL;create;drop"}
------------------------

DDL statements for a table can be created with its `TableQuery`'s `schema` method. Multiple
`DDL` objects can be concatenated with `++` to get a compound `DDL` object which can create
and drop all entities in the correct order, even in the presence of cyclic dependencies between
tables. The `create`, `createIfNotExists`, `dropIfExists`, `drop` and `truncate` methods produce the Actions for executing the DDL statements. To safely create and drop tables use the methods `createIfNotExists` and `dropIfExists` :

```scala src=../code/LiftedEmbedding.scala#ddl
```

You can use the the `statements` method to get the SQL code, like for most other SQL-based
Actions. Schema Actions are currently the only Actions that can produce more than one statement.

```scala src=../code/LiftedEmbedding.scala#ddl2
```
