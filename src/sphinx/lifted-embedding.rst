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
``Column[T]`` (a sub-type of ``Rep[R]``) where a ``TypeMapper[T]`` must
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

Joins
-----

Unions
------

Aggregation
-----------

Querying
--------

Inserting and Updating
----------------------

Query Templates
---------------

User-Defined Functions and Types
--------------------------------
