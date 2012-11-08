Lifted Embedding
================

The *lifted embedding* is the standard API for type-safe queries and updates
in Slick. Please see :doc:`gettingstarted` for an introduction. This chapter
describes the available features in more detail.

The name *Lifted Embedding* refers to the fact that you are not working with
standard Scala types (as in the :doc:`direct embedding <direct-embedding>`)
but with types that are *lifted* into a the ``scala.slick.lifted.Rep`` type
constructor. This becomes clear when you compare the types of a simple
Scala collections example

.. includecode:: code/LiftedEmbedding.scala#plaintypes

... with the types of similar code using the lifted embedding:

.. includecode:: code/LiftedEmbedding.scala#reptypes

All plain types are lifted into ``Rep``. The same is true for the record
type ``Coffees`` which is a subtype of ``Rep[(String, Int, Double, Int, Int)]``.
Even the literal ``8.0`` is automatically lifted to a ``Rep[Double]`` by an
implicit conversion because that is what the ``>`` operator on
``Rep[Double]`` expects for the right-hand side.

Tables
------

In order to use the lifted embedding, you need to define ``Table`` objects
for your database tables:

.. includecode:: code/LiftedEmbedding.scala#tabledef

Note that Slick clones your table objects under the covers, so you should not
add any extra state to them (extra methods are fine though). Also make sure
that an actual ``object`` for a table is not defined in a *static* location
(i.e. at the top level or nested only inside other objects) because this can
cause problems in certain situations due to an overeager optimization performed
by scalac. Using a ``val`` for your table (with an anonymous structural type
or a separate ``class`` definition) is fine everywhere.

All columns are defined through the ``column`` method. Note that they need to
be defined with ``def`` and not ``val`` due to the cloning. Each column has a
Scala type and a column name for the database (usually in upper-case). The
following primitive types are supported out of the box (with certain
limitations imposed by the individual database drivers):

- Boolean
- java.sql.Blob
- Byte
- Array[Byte]
- java.sql.Clob
- java.sql.Date
- Double
- Float
- Int
- Long
- Short
- String
- java.sql.Time
- java.sql.Timestamp
- Unit
- java.util.UUID
- BigDecimal

Nullable columns are represented by ``Option[T]`` where ``T`` is one of the
supported primitive types.

After the column name, you can add optional column options to a ``column``
definition. The applicable options are available through the table's ``O``
object. The following ones are defined for ``BasicProfile``:

``NotNull``, ``Nullable``
   Explicitly mark the column a nullable or non-nullable when creating the
   DDL statements for the table. Nullability is otherwise determined from the
   type (Option or non-Option).

``PrimaryKey``
   Mark the column as a (non-compound) primary key when creating the DDL
   statements.

``Default[T](defaultValue: T)``
   Specify a default value for inserting data the table without this column.
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

Every table requires a ``*`` method contatining a default projection.
This describes what you get back when you return rows (in the form of a
table object) from a query. Slick's ``*`` projection does not have to match
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
``unapply`` method that wraps its result in an ``Option``) but there is also
an overload that operates directly on the mapped types.

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

DDL statements for a table can be created with its ``ddl`` method. Multiple
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

Primitive (non-compound, non-collection) values are representend by type
``Column[T]`` (a sub-type of ``Rep[T]``) where a ``TypeMapper[T]`` must
exist. Only some special methods for internal use and those that deal with
conversions between nullable and non-nullable columns are defined directly in
the ``Column`` class.

The operators and other methods which are commonly used in the lifted
embedding are added through implicit conversions defined in
``ExtensionMethodConversions``. The actual methods can be found in
the classes ``AnyExtensionMethods``, ``ColumnExtensionMethods``,
``NumericColumnExtensionMethods``, ``BooleanColumnExtensionMethods`` and
``StringColumnExtensionMethods``.

Collection values are represented by the ``Query`` class (a ``Rep[Seq[T]]``)
which contains many standard collection methods like ``flatMap``,
``filter``, ``take`` and ``groupBy``. Due to the two different component
types of a ``Query`` (lifted and plain), the signatures for these methods are
very complex but the semantics are essentially the same as for Scala
collections.

Additional methods for queries of non-compound values are added via an
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

The explicit versions of the cross join and inner join will result in the same
SQL code being generated as for the implicit versions (usually an implicit join
in SQL). Note the use of ``.?`` in the outer joins. Since these joins can
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
represented by an SQL database and Slick does not currently support it, either
(but this is expected to change in the future). The resulting zipped query,
however, can be represented in SQL with the use of a *row number* function,
so ``zipWithIndex`` is supported as a primitive operator:

.. includecode:: code/JoinsUnions.scala#zipWithIndex

Unions
------

Two queries can be concatenated with the ``union`` and ``unionAll`` operators
if they have compatible types:

.. includecode:: code/JoinsUnions.scala#union

Unlike ``union`` which filters out duplicate values, ``unionAll`` simply
concatenates the queries, which is usually more efficient.

Aggregation
-----------

The simplest form of aggregation consists of computing a primitive value from a
Query that returns a single column, usually with a numeric type, e.g.:

.. includecode:: code/LiftedEmbedding.scala#aggregation1

Some aggregation functions are defined for arbitrary queries:

.. includecode:: code/LiftedEmbedding.scala#aggregation2

Grouping is done with the ``groupBy`` method. It has the same semantics as for
Scala collections:

.. includecode:: code/LiftedEmbedding.scala#aggregation3

Note that the intermediate query ``q`` contains nested values of type ``Query``.
These would turn into nested collections when executing the query, which is
not supported at the moment. Therefore it is necessary to flatten the nested
queries by aggregating their values (or individual columns) as done in ``q2``.


Querying
--------

Inserting and Updating
----------------------

Query Templates
---------------

User-Defined Functions and Types
--------------------------------
