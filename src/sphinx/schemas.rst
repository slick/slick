Schemas
=======

This chapter describes how to work with database schemas in the
:ref:`Lifted Embedding <lifted-embedding>` API. This explains
how you can write schema descriptions by hand. Instead you 
can also use the :doc:`code generator <code-generation>` to 
take this work off your hands.

Table Rows
----------

In order to use the *Lifted Embedding* API for type-safe queries, you need to
define ``Table`` row classes for your database schema. These describe the
structure of the tables:

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

If your database layout requires *schema names*, you can specify the schema
name for a table in front of the table name, wrapped in ``Some()``:

.. includecode:: code/LiftedEmbedding.scala#schemaname

Table Query
-----------

Alongside the ``Table`` row class you also need a ``TableQuery`` value
which represents the actual database table:

.. includecode:: code/LiftedEmbedding.scala#tablequery

The simple ``TableQuery[T]`` syntax is a
macro which expands to a proper TableQuery instance that calls the table's
constructor (``new TableQuery(new T(_))``).

You can also extend ``TableQuery`` to use it as a convenient namespace for
additional functionality associated with the table:

.. includecode:: code/LiftedEmbedding.scala#tablequery2

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

For case classes with hand-written companion objects, ``.tupled`` only works
if you manually extend the correct Scala function type. Alternatively you can use
``(User.apply _).tupled``. See `SI-3664 <https://issues.scala-lang.org/browse/SI-3664>`_ and
`SI-4808 <https://issues.scala-lang.org/browse/SI-4808>`_.

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
