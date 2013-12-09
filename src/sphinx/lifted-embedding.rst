Lifted Embedding
================

The *lifted embedding* is the standard API for type-safe queries and updates
in Slick. Please see :doc:`gettingstarted` for an introduction. This chapter
describes the available features in more detail.

The name *Lifted Embedding* refers to the fact that you are not working with
standard Scala types (as in the :doc:`direct embedding <direct-embedding>`)
but with types that are *lifted* into a :api:`scala.slick.lifted.Rep` type
constructor. This becomes clear when you compare the types of a simple
Scala collections example

.. includecode:: code/LiftedEmbedding.scala#plaintypes

... with the types of similar code using the lifted embedding:

.. includecode:: code/LiftedEmbedding.scala#reptypes

All plain types are lifted into ``Rep``. The same is true for the table row
type ``Coffees`` which is a subtype of ``Rep[(String, Double)]``.
Even the literal ``8.0`` is automatically lifted to a ``Rep[Double]`` by an
implicit conversion because that is what the ``>`` operator on
``Rep[Double]`` expects for the right-hand side. This lifting is necessary
because the lifted types allow us to generate a syntax tree that captures
the query computations. Getting plain Scala functions and values would not
give us enough information for translating those computations to SQL.

Tables
------

In order to use the lifted embedding, you need to define ``Table`` row classes
and corresponding ``TableQuery`` values for your database tables:

.. includecode:: code/LiftedEmbedding.scala#tabledef

All columns are defined through the ``column`` method. Each column has a
Scala type and a column name for the database (usually in upper-case). The
following primitive types are supported out of the box for JDBC-based
databases in ``JdbcProfile`` (with certain limitations imposed by the
individual database drivers):

- *Numeric types*: Byte, Short, Int, Long, BigDecimal, Float, Double
- *LOB types*: java.sql.Blob, java.sql.Clob, Array[Byte]
- *Date types*: java.sql.Date, java.sql.Time, java.sql.Timestamp
- Boolean
- String
- Unit
- java.util.UUID

Nullable columns are represented by ``Option[T]`` where ``T`` is one of the
supported primitive types. Note that all operations on Option values are
currently using the database's null propagation semantics which may differ
from Scala's Option semantics. In particular, ``None === None`` evaluates
to ``None``. This behaviour may change in a future major release of Slick.

After the column name, you can add optional column options to a ``column``
definition. The applicable options are available through the table's ``O``
object. The following ones are defined for ``JdbcProfile``:

``PrimaryKey``
   Mark the column as a (non-compound) primary key when creating the DDL
   statements.

``Default[T](defaultValue: T)``
   Specify a default value for inserting data into the table without this column.
   This information is only used for creating DDL statements so that the
   database can fill in the missing information.

``DBType(dbType: String)``
   Use a non-standard database-specific type for the DDL statements (e.g.
   ``DBType("VARCHAR(20)")`` for a ``String`` column).

``AutoInc``
   Mark the column as an auto-incrementing key when creating the DDL
   statements. Unlike the other column options, this one also has a meaning
   outside of DDL creation: Many databases do not allow non-AutoInc columns to
   be returned when inserting data (often silently ignoring other columns), so
   Slick will check if the return column is properly marked as AutoInc where
   needed.

``NotNull``, ``Nullable``
   Explicitly mark the column as nullable or non-nullable when creating the
   DDL statements for the table. Nullability is otherwise determined from the
   type (Option or non-Option). There is usually no reason to specify these
   options.

Every table requires a ``*`` method contatining a default projection.
This describes what you get back when you return rows (in the form of a
table row object) from a query. Slick's ``*`` projection does not have to match
the one in the database. You can add new columns (e.g. with computed values)
or omit some columns as you like. The non-lifted type corresponding to the
``*`` projection is given as a type parameter to ``Table``. For simple,
non-mapped tables, this will be a single column type or a tuple of column
types.

Mapped Tables
-------------

It is possible to define a mapped table that uses a custom type for its ``*``
projection by adding a bi-directional mapping with the ``<>`` operator:

.. includecode:: code/LiftedEmbedding.scala#mappedtable

It is optimized for case classes (with a simple ``apply`` method and an
``unapply`` method that wraps its result in an ``Option``) but it can also
be used with arbitrary mapping functions. In these cases it can be useful
to call ``.shaped`` on a tuple on the left-hand side in order to get its
type inferred properly. Otherwise you may have to add full type annotations
to the mapping functions.

Constraints
-----------

A foreign key constraint can be defined with a table's ``foreignKey`` method.
It takes a name for the constraint, the local column (or projection, so you
can define compound foreign keys), the linked table, and a function from that
table to the corresponding column(s). When creating the DDL statements for the
table, the foreign key definition is added to it.

.. includecode:: code/LiftedEmbedding.scala#foreignkey

Independent of the actual constraint defined in the database, such a foreign
key can be used to navigate to the linked data with a *join*. For this
purpose, it behaves the same as a manually defined utility method for finding
the joined data:

.. includecode:: code/LiftedEmbedding.scala#foreignkeynav

A primary key constraint can be defined in a similar fashion by adding a
method that calls ``primaryKey``. This is useful for defining compound
primary keys (which cannot be done with the ``O.PrimaryKey`` column option):

.. includecode:: code/LiftedEmbedding.scala#primarykey

Other indexes are defined in a similar way with the ``index`` method. They
are non-unique by default unless you set the ``unique`` parameter:

.. includecode:: code/LiftedEmbedding.scala#index

All constraints are discovered reflectively by searching for methods with
the appropriate return types which are defined in the table. This behavior
can be customized by overriding the ``tableConstraints`` method.

Data Definition Language
------------------------

DDL statements for a table can be created with its ``TableQuery``"s ``ddl``
method. Multiple
``DDL`` objects can be concatenated with ``++`` to get a compound ``DDL``
object which can create and drop all entities in the correct order, even in
the presence of cyclic dependencies between tables. The statements are
executed with the ``create`` and ``drop`` methods:

.. includecode:: code/LiftedEmbedding.scala#ddl

You can use the ``createStatements`` and ``dropStatements`` methods to get
the SQL code:

.. includecode:: code/LiftedEmbedding.scala#ddl2

Expressions
-----------

Scalar (non-record, non-collection) values are representend by type
``Column[T]`` (a sub-type of ``Rep[T]``) where a ``TypedType[T]`` must
exist. Only some special methods for internal use are defined directly in
the ``Column`` class.

The operators and other methods which are commonly used in the lifted
embedding are added through implicit conversions defined in
``ExtensionMethodConversions``. The actual methods can be found in
the classes ``AnyExtensionMethods``, ``ColumnExtensionMethods``,
``NumericColumnExtensionMethods``, ``BooleanColumnExtensionMethods`` and
``StringColumnExtensionMethods``
(cf. :slick:`ExtensionMethods <src/main/scala/scala/slick/lifted/ExtensionMethods.scala>`).

Collection values are represented by the ``Query`` class (a ``Rep[Seq[T]]``)
which contains many standard collection methods like ``flatMap``,
``filter``, ``take`` and ``groupBy``. Due to the two different component
types of a ``Query`` (lifted and plain), the signatures for these methods are
very complex but the semantics are essentially the same as for Scala
collections.

Additional methods for queries of scalar values are added via an
implicit conversion to ``SingleColumnQueryExtensionMethods``.

Sorting and Filtering
---------------------

There are various methods with sorting/filtering semantics (i.e. they take a
``Query`` and return a new ``Query`` of the same type), for example:

.. includecode:: code/LiftedEmbedding.scala#filtering

Joining and Zipping
-------------------

Joins are used to combine two different tables or queries into a single query.

There are two different ways of writing joins: *Explicit* joins are performed
by calling a method that joins two queries into a single query of a tuple of
the individual results. *Implicit* joins arise from a specific shape of a query
without calling a special method.

An *implicit cross-join* is created with a ``flatMap`` operation on a ``Query``
(i.e. by introducing more than one generator in a for-comprehension):

.. includecode:: code/JoinsUnions.scala#implicitCross

If you add a filter expression, it becomes an *implicit inner join*:

.. includecode:: code/JoinsUnions.scala#implicitInner

The semantics of these implicit joins are the same as when you are using
``flatMap`` on Scala collections.

Explicit joins are created by calling one of the available join methods:

.. includecode:: code/JoinsUnions.scala#explicit

Note the use of ``.?`` in the outer joins. Since these joins can
introduce additional NULL values (on the right-hand side for a left outer join,
on the left-hand sides for a right outer join, and on both sides for a full
outer join), you have to make sure to retrieve ``Option`` values from them.

In addition to the usual join operators supported by relational databases
(which are based off a cross join or outer join), Slick also has *zip joins*
which create a pairwise join of two queries. The semantics are again the same
as for Scala collections, using the ``zip`` and ``zipWith`` methods:

.. includecode:: code/JoinsUnions.scala#zip

A particular kind of zip join is provided by ``zipWithIndex``. It zips a query
result with an infinite sequence starting at 0. Such a sequence cannot be
represented by an SQL database and Slick does not currently support it, either.
The resulting zipped query,
however, can be represented in SQL with the use of a *row number* function,
so ``zipWithIndex`` is supported as a primitive operator:

.. includecode:: code/JoinsUnions.scala#zipWithIndex

Unions
------

Two queries can be concatenated with the ``++`` (or ``unionAll``) and ``union``
operators if they have compatible types:

.. includecode:: code/JoinsUnions.scala#union

Unlike ``union`` which filters out duplicate values, ``++`` simply concatenates
the results of the individual queries, which is usually more efficient.

Aggregation
-----------

The simplest form of aggregation consists of computing a primitive value from a
Query that returns a single column, usually with a numeric type, e.g.:

.. includecode:: code/LiftedEmbedding.scala#aggregation1

Note that these aggregate queries return a scalar result, not a collection.
Some aggregation functions are defined for arbitrary queries (of more than
one column):

.. includecode:: code/LiftedEmbedding.scala#aggregation2

Grouping is done with the ``groupBy`` method. It has the same semantics as for
Scala collections:

.. includecode:: code/LiftedEmbedding.scala#aggregation3

Note that the intermediate query ``q`` contains nested values of type ``Query``.
These would turn into nested collections when executing the query, which is
not supported at the moment. Therefore it is necessary to flatten the nested
queries immediately by aggregating their values (or individual columns)
as done in ``q2``.

Querying
--------

Queries are executed using methods defined in the :api:`scala.slick.jdbc.Invoker`
trait (or :api:`scala.slick.jdbc.UnitInvoker` for the parameterless versions).
There is an implicit conversion from ``Query``, so you can execute any
``Query`` directly. The most common usage scenario is reading a complete
result set into a strict collection with a specialized method such as ``list``
or the generic method ``to`` which can build any kind of collection:

.. includecode:: code/LiftedEmbedding.scala#invoker

This snippet also shows how you can get a reference to the invoker without
having to call the implicit conversion method manually.

All methods that execute a query take an implicit ``Session`` value. Of
course, you can also pass a session explicitly if you prefer:

.. includecode:: code/LiftedEmbedding.scala#invoker_explicit

If you only want a single result value, you can use ``first`` or
``firstOption``. The methods ``foreach``, ``foldLeft`` and ``elements`` can be
used to iterate over the result set without first copying all data into a
Scala collection.

Deleting
--------

Deleting works very similarly to querying. You write a query which selects the
rows to delete and then call the ``delete`` method on it. There is again an
implicit conversion from ``Query`` to the special
:api:`scala.slick.driver.BasicInvokerComponent$DeleteInvoker` which provides
the ``delete`` method and a self-reference ``deleteInvoker``:

.. includecode:: code/LiftedEmbedding.scala#delete

A query for deleting must only select from a single table. Any projection is
ignored (it always deletes full rows).

Inserting
---------

Inserts are done based on a projection of columns from a single table. When
you use the table directly, the insert is performed against its ``*``
projection. Omitting some of a table's columns when inserting causes the
database to use the default values specified in the table definition, or
a type-specific default in case no explicit default was given. All methods
for inserting are defined in
:api:`scala.slick.driver.BasicInvokerComponent$InsertInvoker` and
:api:`scala.slick.driver.BasicInvokerComponent$FullInsertInvoker`.

.. includecode:: code/LiftedEmbedding.scala#insert1

When you include an ``AutoInc`` column in an insert operation, it is silently
ignored, so that the database can generate the proper value.
In this case you usually want to get back the auto-generated primary key
column. By default, ``+=`` gives you a count of the number of affected
rows (which will usually be 1) and ``++=`` gives you an accumulated
count in an ``Option`` (which can be ``None`` if the database system does not
provide counts for all rows). This can be changed with the ``returning``
method where you specify the columns to be returned (as a single value or
tuple from ``+=`` and a ``Seq`` of such values from ``++=``):

.. includecode:: code/LiftedEmbedding.scala#insert3

Note that many database systems only allow a single column to be returned
which must be the table's auto-incrementing primary key. If you ask for
other columns a ``SlickException`` is thrown at runtime (unless the database
actually supports it).

Instead of inserting data from the client side you can also insert data
created by a ``Query`` or a scalar expression that is executed in the
database server:

.. includecode:: code/LiftedEmbedding.scala#insert4

In these cases, ``AutoInc`` columns are *not* ignored.

Updating
--------

Updates are performed by writing a query that selects the data to update and
then replacing it with new data. The query must only return raw columns (no
computed values) selected from a single table. The relevant methods for
updating are defined in
:api:`scala.slick.driver.BasicInvokerComponent$UpdateInvoker`.

.. includecode:: code/LiftedEmbedding.scala#update1

There is currently no way to use scalar expressions or transformations of
the existing data in the database for updates.

Compiled Queries
----------------

Database queries typically depend on some parameters, e.g. an ID for which
you want to retrieve a matching database row. You can write a regular Scala
function to create a parameterized ``Query`` object each time you need to
execute that query but this will incur the cost of recompiling the query
in Slick (and possibly also on the database if you don't use bind variables
for all parameters). It is more efficient to pre-compile such parameterized
query functions:

.. includecode:: code/LiftedEmbedding.scala#compiled1

This works for all functions that take ``Column`` parameters (or
:ref:`records <record-types>` of Columns) and return a ``Query`` object or a
scalar query. See the API documentation for :api:`scala.slick.lifted.Compiled`
and its subclasses for details on composing compiled queries.

You can use a compiled query for querying, updating and deleting data.

For backwards-compatibility with Slick 1.0 you can still create a compiled
query by calling ``flatMap`` on a :api:`scala.slick.lifted.Parameters` object.
In many cases this enables you to write a single *for comprehension* for a
compiled query:

.. includecode:: code/LiftedEmbedding.scala#template1

User-Defined Functions
----------------------

If your database system supports a scalar function that is not available as
a method in Slick you can define it as a
:api:`scala.slick.lifted.SimpleFunction`. There are predefined methods for
creating unary, binary and ternary functions with fixed parameter and return
types.

.. includecode:: code/LiftedEmbedding.scala#simplefunction1

If you need more flexibility regarding the types (e.g. for varargs,
polymorphic functions, or to support Option and non-Option types in a single
function), you can use ``SimpleFunction.apply`` to get an untyped instance and
write your own wrapper function with the proper type-checking:

.. includecode:: code/LiftedEmbedding.scala#simplefunction2

:api:`scala.slick.lifted.SimpleBinaryOperator` and
:api:`scala.slick.lifted.SimpleLiteral` work in a similar way. For even more
flexibility (e.g. function-like expressions with unusual syntax), you can
use :api:`scala.slick.lifted.SimpleExpression`.

User-Defined Scalar Types
-------------------------

If you need a custom column type you can implement
:api:`ColumnType <scala.slick.driver.JdbcProfile@ColumnType>`. The most
common scenario is mapping an application-specific type to an already supported
type in the database. This can be done much simpler by using
:api:`MappedColumnType <scala.slick.driver.JdbcProfile@MappedColumnType>` which
takes care of all the boilerplate:

.. includecode:: code/LiftedEmbedding.scala#mappedtype1

You can also subclass
:api:`MappedJdbcType <scala.slick.driver.JdbcProfile@MappedJdbcType>`
for a bit more flexibility.

If you have a wrapper class (which can optionally be a case class and/or value
class) for an underlying value of some supported type, you can make it extend
:api:`scala.slick.lifted.MappedTo` to get a macro-generated implicit
``ColumnType`` for free. Such wrapper classes are commonly used for type-safe
table-specific primary key types:

.. includecode:: code/LiftedEmbedding.scala#mappedtype2


.. _record-types:

User-Defined Record Types
-------------------------

Out of the box, Slick supports Scala tuples (up to arity 22) and Slick's own
experimental :api:`scala.slick.collection.heterogenous.HList` implementation
(without any size limit) for record types, i.e. data structures containing a
fixed number of elements with individually defined types. These record types
can be nested and mixed arbitrarily.

If you need more flexibility, you can add implicit :api:`scala.slick.lifted.Shape`
definitions for other types like ``Pair`` in this example:

.. includecode:: code/LiftedEmbedding.scala#recordtypepair

``Shape`` implementations for record types extend
:api:`scala.slick.lifted.MappedScalaProductShape`. They are are generally very
simple but they require some boilerplate for all the types involved. A
``MappedScalaProductShape`` takes a sequence of Shapes for its elements and
provides the operations ``buildValue`` (for creating an instance of the record
type given its elements) and ``copy`` (for creating a copy of this ``Shape``
with new element Shapes):

.. includecode:: code/LiftedEmbedding.scala#recordtype1

The implicit method ``pairShape`` in this example provides a Shape for a
``Pair`` of two element types whenever Shapes for the inidividual element
types are available.

With these definitions in place, we can use the ``Pair`` record type in every
location in Slick where a tuple or ``HList`` would be acceptable:

.. includecode:: code/LiftedEmbedding.scala#recordtype2
