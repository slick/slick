.. index:: Query

Queries
=======

This chapter describes how to write type-safe queries for selecting,
inserting, updating and deleting data with the
:ref:`Lifted Embedding <lifted-embedding>` API.

.. index:: expression, Column, scalar, collection-valued, Rep, Seq, extension method, select, projection, view

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

.. index:: sort, filter, order by, sortBy, where

Sorting and Filtering
---------------------

There are various methods with sorting/filtering semantics (i.e. they take a
``Query`` and return a new ``Query`` of the same type), for example:

.. includecode:: code/LiftedEmbedding.scala#filtering

.. index:: join, zip, zipWithIndex

Joining and Zipping
-------------------

Joins are used to combine two different tables or queries into a single query.

There are two different ways of writing joins: *Explicit* joins are performed
by calling a method that joins two queries into a single query of a tuple of
the individual results. *Implicit* joins arise from a specific shape of a query
without calling a special method.

.. index::
   pair: join; implicit
   pair: join; inner
   pair: join; cross

An *implicit cross-join* is created with a ``flatMap`` operation on a ``Query``
(i.e. by introducing more than one generator in a for-comprehension):

.. includecode:: code/JoinsUnions.scala#implicitCross

If you add a filter expression, it becomes an *implicit inner join*:

.. includecode:: code/JoinsUnions.scala#implicitInner

The semantics of these implicit joins are the same as when you are using
``flatMap`` on Scala collections.

.. index::
   pair: join; outer
   pair: join; explicit

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

.. index:: union, ++, unionAll

Unions
------

Two queries can be concatenated with the ``++`` (or ``unionAll``) and ``union``
operators if they have compatible types:

.. includecode:: code/JoinsUnions.scala#union

Unlike ``union`` which filters out duplicate values, ``++`` simply concatenates
the results of the individual queries, which is usually more efficient.

.. index:: aggregate, min, max, sum, avg, length, count, exists

Aggregation
-----------

The simplest form of aggregation consists of computing a primitive value from a
Query that returns a single column, usually with a numeric type, e.g.:

.. includecode:: code/LiftedEmbedding.scala#aggregation1

Note that these aggregate queries return a scalar result, not a collection.
Some aggregation functions are defined for arbitrary queries (of more than
one column):

.. includecode:: code/LiftedEmbedding.scala#aggregation2

.. index:: group by, groupBy

Grouping is done with the ``groupBy`` method. It has the same semantics as for
Scala collections:

.. includecode:: code/LiftedEmbedding.scala#aggregation3

Note that the intermediate query ``q`` contains nested values of type ``Query``.
These would turn into nested collections when executing the query, which is
not supported at the moment. Therefore it is necessary to flatten the nested
queries immediately by aggregating their values (or individual columns)
as done in ``q2``.

.. index:: querying, Invoker, first, buildColl, selectStatement, list
.. index::
   pair: query; execute
   pair: query; run

Querying
--------

Queries are executed using methods defined in the :api:`scala.slick.jdbc.Invoker`
trait. There is an implicit conversion from ``Query``, so you can execute any
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

.. index:: delete, DeleteInvoker, deleteStatement

Deleting
--------

Deleting works very similarly to querying. You write a query which selects the
rows to delete and then call the ``delete`` method on it. There is again an
implicit conversion from ``Query`` to the special
:api:`DeleteInvoker <scala.slick.driver.JdbcInvokerComponent@DeleteInvoker:JdbcDriver.DeleteInvoker>` which provides
the ``delete`` method and a self-reference ``deleteInvoker``:

.. includecode:: code/LiftedEmbedding.scala#delete

A query for deleting must only select from a single table. Any projection is
ignored (it always deletes full rows).

.. index:: insert, +=, ++=, InsertInvoker, insertStatement

Inserting
---------

Inserts are done based on a projection of columns from a single table. When
you use the table directly, the insert is performed against its ``*``
projection. Omitting some of a table's columns when inserting causes the
database to use the default values specified in the table definition, or
a type-specific default in case no explicit default was given. All methods
for inserting are defined in
:api:`InsertInvoker <scala.slick.driver.JdbcInsertInvokerComponent@InsertInvokerDef[U]:JdbcDriver.InsertInvokerDef[U]>` and
:api:`FullInsertInvoker <scala.slick.driver.JdbcInsertInvokerComponent@FullInsertInvokerDef[U]:JdbcDriver.FullInsertInvokerDef[U]>`.

.. includecode:: code/LiftedEmbedding.scala#insert1

.. index:: returning, AutoInc, generated key, into

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

You can follow the ``returning`` method with the ``into`` method to map
the inserted values and the generated keys (specified in returning) to a desired value.
Here is an example of using this feature to return an object with an updated id:

.. includecode:: code/LiftedEmbedding.scala#insert3b

Instead of inserting data from the client side you can also insert data
created by a ``Query`` or a scalar expression that is executed in the
database server:

.. includecode:: code/LiftedEmbedding.scala#insert4

In these cases, ``AutoInc`` columns are *not* ignored.

.. index:: update, UpdateInvoker, updateStatement

Updating
--------

Updates are performed by writing a query that selects the data to update and
then replacing it with new data. The query must only return raw columns (no
computed values) selected from a single table. The relevant methods for
updating are defined in
:api:`UpdateInvoker <scala.slick.driver.JdbcInvokerComponent@UpdateInvoker[T]:JdbcDriver.UpdateInvoker[T]>`.

.. includecode:: code/LiftedEmbedding.scala#update1

There is currently no way to use scalar expressions or transformations of
the existing data in the database for updates.

.. index:: prepared, QueryTemplate, parameter
.. index::
   pair: query; compiled
.. _compiled-queries:

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

You can use a compiled query for querying, updating and deleting data. (For inserts,
you can cache the :api:`InsertInvoker <scala.slick.driver.JdbcInsertInvokerComponent@InsertInvokerDef[U]:JdbcDriver.InsertInvokerDef[U]>`
and re-use it instead. To get it, call a query's
:api:`insertInvoker <scala.slick.profile.BasicInsertInvokerComponent$InsertInvokerDef@insertInvoker:InsertInvokerDef.this.type>`
method, which is added by the
:api:`createInsertInvoker <scala.slick.driver.JdbcInsertInvokerComponent@createInsertInvoker[U](tree:JdbcInsertInvokerComponent.this.CompiledInsert):JdbcInsertInvokerComponent.this.CountingInsertInvokerDef[U]>`
implicit conversion.)

For backwards-compatibility with Slick 1.0 you can still create a compiled
query by calling ``flatMap`` on a :api:`scala.slick.lifted.Parameters` object.
In many cases this enables you to write a single *for comprehension* for a
compiled query:

.. includecode:: code/LiftedEmbedding.scala#template1
