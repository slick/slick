.. index:: migration, 1.0, upgrading

Upgrade guides
##############

Upgrade from 2.1 to 3.0
=======================

Database I/O Actions
--------------------

The ``simple`` and ``Implicits`` imports from drivers are deprecated. You should use ``api`` instead, which will give
you the same features, except for the old ``Invoker`` and ``Executor`` APIs for blocking execution of database calls.
These have been replaced by a new monadic database I/O actions API. See :doc:`Databases & I/O Actions <database>` for
details of the new API.

Join Operators
--------------

The old outer join operators did not handle ``null`` values correctly, requiring complicated mappings in user code,
especially when using nested outer joins or outer joins over mapped entities. This is no longer necessary with the
new outer join operators that lift one (left or right outer join) or both sides (full outer join) of the join into an
``Option``. This is made possible by the new nested Options and non-primitive Options support in Slick.

The old operators are deprecated but still available. Deprecation warnings will point you to the right replacement:

- leftJoin -> joinLeft
- rightJoin -> joinRight
- outerJoin -> joinFull
- innerJoin -> join

Passing an explicit ``JoinType`` to the generic ``join`` operator does not make sense anymore with the new join
semantics and is therefore deprecated, too. ``join`` is now used exclusively for inner joins.

first
-----

The old Invoker API used the ``first`` and ``firstOption`` methods to get the first element of a collection-valued
query. The same operations for streaming Actions in the new API are called ``head`` and ``headOption`` respectively,
consistent with the names used by the Scala Collections API.

Column Type
-----------

The type ``Column[T]`` has been subsumed into its supertype ``Rep[T]``. For operations which are only available for
individual columns, an implicit ``TypedType[T]`` evidence is required. The more flexible handling of Option columns
requires Option and non-Option columns to be treated differently when creating an implicit ``Shape``. In this case a
the evidence needs to be of type ``OptionTypedType[T]`` or ``BaseTypedType[T]``, respectively. If you want to abstract
over both, it may be more convenient to pass the required ``Shape`` as an implicit parameter and let it be instantiated
at the call site where the concrete type is known.

``Column[T]`` is still available as a deprecated alias for ``Rep[T]``. Due to the required implicit evidence, it
cannot provide complete backwards compatibility in all cases.

Closing Databases
-----------------

Since a ``Database`` instance can now have an associated connection pool and thread pool, it is important to call
``close`` when you are done using it, so that these pools can be shut down properly. You should take care to do this
wen you migrate to the new Action-based API. As long as you exclusively use the old API (and do not configure a
session pool), it is not strictly necessary.

Metadata API and Code Generator
-------------------------------

The JDBC metadata API in package ``scala.slick.jdbc.meta`` has been switched to the new API, producing Actions instead
of Invokers. The code generator, which uses this API, has been completely rewritten for the asynchronous API. It still
supports the same functionality and the same concepts but any customization of the code generator will have to be
changed. See the code generator tests and the :doc:`code-generation` chapter for examples.

Inserting from Queries and Expressions
--------------------------------------

In Slick 2.0, soft inserts became the default for inserting raw values. Inserting from another query or a computed
expression still uses force-insert semantics. The new DBIO API properly reflects this by renaming ``insert(Query)``
to ``forceInsertQuery(Query)`` and ``insertExpr`` to ``forceInsertExpr``.

Upgrade from 2.0 to 2.1
=======================

Query type parameters
---------------------
:api:`Query <scala.slick.lifted.Query>` now takes 3 type parameters instead of two. 2.0's ``Query[T,E]`` is equivalent to Slick 2.1's ``Query[T,E,Seq]``. The third parameter is the collection type to be returned when executing the query using ``.run``, which always returned a ``Seq`` in Slick 2.0. This is the only place where it is used right now. In the future we will work on making queries correspond to the behavior of the corresponding Scala collection types, i.e. ``Query[_,_,Set]`` having the uniqueness property, ``Query[_,_,List]`` being order preserving, etc. The collecton type can be changed to ``C`` by calling ``.to[C]`` on a query.

To upgrade your code to 2.1 you can either rename the new Query type to something else in the import, i.e. ``import ....simple.{Query=>NewQuery,_}`` and then write a type alias ``type Query[T,E] = NewQuery[T,E,Seq]``. Or you can add ``Seq`` as the third type argument in your code. This regex should work for most places: replace ``([^a-zA-Z])Query\[([^\]]+), ?([^\]]+)\]`` with ``\1Query[\2, \3, Seq]``.

``.list`` and ``.first``
------------------------
These methods had an empty argument list before the implicit argument list in 2.0. This has been dropped for uniformity. Calls like ``.list()`` need to be replaced with ``.list`` and ``.first()`` by ``.first``.

``.where``
----------
This method has been deprecated in favor of the Scala collections conformant ``.filter`` method.

``.isNull`` and ``.isNotNull``
------------------------------
These methods have been deprecated in favor of new Scala standard library conformant ``isEmpty`` and ``isDefined`` methods. They can now only be used on Option columns. Otherwise you get a type error. A quick workaround for using them on non-Option columns is casting them into Option columns using ``.?``, e.g. ``someCol.?.isDefined``. The reason that you have to do this points to using a wrong type for your column however, i.e. non-Option for a nullable column and should really be fixed in your Table definition.

Static Plain SQL Queries
------------------------
The interface for using argument placeholders has been changed. Where in 2.0 you could write

``StaticQuery.query[T,…]("select ...").list(someT)``

you now have to write

``StaticQuery.query[T,…]("select ...").apply(someT).list``

Slick code generator / Slick model
----------------------------------
The code generator has been moved into a separate artifact in order to evolve it faster than Slick core. it moved from package ``scala.slick.model.codegen`` to package ``scala.slick.codegen``. Binary compatibility will not be guaranteed, as it is supposed to be used before compile time. Add

.. parsed-literal::
  "com.typesafe.slick" %% "slick-codegen" % "|release|"

to the dependencies of your code generator sbt project.

Method ``SourceCodeGenerator#Table#compound`` has been replaced by two methods ``compoundValue`` and ``compoundType`` generating potentially differently shaped code for values and types of compound values.

Method ``getTables`` of the Slick drivers, which returns an Invoker for listing all default database tables has been deprecated in favor of new method ``defautTables``, which returns the tables directly in order to allow Slick to exclude meta tables at this point.

Method ``scala.slick.jdbc.meta.createModel(tables)`` has been moved into the drivers and can now be invoked using e.g. ``H2Driver.createModel(Some(tables))``

The model generated by Slick now contains improved information like the database column type, length of string columns, default values for strings in MySQL. The code generator
will embed the portable length into generated code and can optionally embed the non-portable database column type into generated code when overriding ``SlickCodeGenerator#Table#Column#dbType`` with ``true``.

The new ``ModelBuilder`` can be extended to customize model creation from jdbc meta data, similar to how the code generator can be customized. This allows working around differences and bugs in jdbc drivers, when creating the model or making up for missing features in Slick, e.g supporting specific types of your dbms of choice.

Upgrade from 1.0 to 2.0
=======================

Slick 2.0 contains some improvements which are not source compatible with Slick
1.0. When migrating your application from 1.0 to 2.0, you will likely need to
perform changes in the following areas.

Code Generation
---------------

Instead of writing your table descriptions or plain SQL mappers by hand, in 2.0 you can
now automatically generate them from your database schema. The code-generator
is flexible enough to customize it's output to fit exactly what you need.
:doc:`More info on code generation <code-generation>`.

.. index:: table object, ~, tuple

Table Descriptions
------------------

In Slick 1.0 tables were defined by a single ``val`` or ``object`` (called the
*table object*) and the ``*`` projection was limited to a flat tuple of columns
that had to be constructed with the special ``~`` operator::

  // --------------------- Slick 1.0 code -- does not compile in 2.0 ---------------------

  object Suppliers extends Table[(Int, String, String)]("SUPPLIERS") {
    def id = column[Int]("SUP_ID", O.PrimaryKey)
    def name = column[String]("SUP_NAME")
    def street = column[String]("STREET")
    def * = id ~ name ~ street
  }

In Slick 2.0 you need to define your table as a class that takes an extra
``Tag`` argument (the *table row class*) plus an instance of a ``TableQuery``
of that class (representing the actual database table). Tuples for the ``*``
projection can use the standard tuple syntax:

.. includecode:: code/MigrationGuide.scala#tabledef

You can import :api:`TupleMethods <scala.slick.util.TupleMethods$>`._ to get
support for the old `~` syntax. The simple ``TableQuery[T]`` syntax is a
macro which expands to a proper TableQuery instance that calls the table's
constructor (``new TableQuery(new T(_))``). In Slick 1.0 it was common practice
to place extra static methods associated with a table into that table's object.
You can do the same in 2.0 with a custom ``TableQuery`` object:

.. includecode:: code/MigrationGuide.scala#tablequery

Note that a ``TableQuery`` is a ``Query`` for the table. The implicit
conversion from a table row object to a ``Query`` that could be applied in
unexpected places is no longer needed or available. All the places where you
had to use the raw *table object* in Slick 1.0 have been changed to use the
*table query* instead, e.g. inserting (see below) or foreign key references.

The method for creating simple finders has been renamed from ``createFinderBy``
to ``findBy``. It is defined as an *extension method* for ``TableQuery``, so
you have to prefix the call with ``this.`` (see code snippet above).

Mapped Tables
-------------

In 1.0 the ``<>`` method for bidirectional mappings was overloaded for
different arities so you could directly pass a case class's ``apply`` method to
it::

  // --------------------- Slick 1.0 code -- does not compile in 2.0 ---------------------

  def * = id ~ name ~ street <> (Supplier _, Supplier.unapply)

This is no longer supported in 2.0. One of the reasons is that the overloading
led to complicated error messages.
You now have to use a function with an appropriate tuple type.
If you map to a case class you can simply use ``.tupled`` on its
companion object:

.. includecode:: code/MigrationGuide.scala#mappedprojection

Note that ``.tupled`` is only available for proper Scala *functions*. In 1.0 it
was sufficient to have a *method* like ``apply`` that could be converted to
a function on demand (``<> (Supplier.apply _, Supplier.unapply)``).

When using a case class, the companion object extends the correct function
type by default, but only if you do not define the object yourself. In that
case you should provide the right supertype manually, e.g.:

.. includecode:: code/MigrationGuide.scala#caseclassextends

Alternatively, you can have the Scala compiler first do the lifting to a
function and then call ``.tupled``:

.. includecode:: code/MigrationGuide.scala#mappedprojection2

.. index:: profile, BasicProfile, ExtendedProfile, JdbcProfile

Profile Hierarchy
-----------------

Slick 1.0 provided two *profiles*, ``BasicProfile`` and ``ExtendedProfile``.
These two have been unified in 2.0 as ``JdbcProfile``. Slick now provides
more abstract profiles, in particular ``RelationalProfile`` which does not
have all the features of ``JdbcProfile`` but is supported by the new
``HeapDriver`` and ``DistributedDriver``. When porting code from Slick 1.0,
you generally want to switch to ``JdbcProfile`` when abstracting over
drivers. In particular, pay attention to the fact that ``BasicProfile``
in 2.0 is very different from ``BasicProfile`` in 1.0.

Inserting
---------

In Slick 1.0 you used to construct a projection for inserting from the
*table object*::

  // --------------------- Slick 1.0 code -- does not compile in 2.0 ---------------------

  (Suppliers.name ~ Suppliers.street) insert ("foo", "bar")

Since there is no raw table object any more in 2.0 you have to use a
projection from the table query:

.. includecode:: code/MigrationGuide.scala#insert1

Note the use of the new ``+=`` operator for API compatibility with Scala
collections. The old name ``insert`` is still available as an alias.

Slick 2.0 will now automatically exclude ``AutoInc`` fields by default when
inserting data. In 1.0 it was common to have a separate projection for
inserts in order to exclude these fields manually::

  // --------------------- Slick 1.0 code -- does not compile in 2.0 ---------------------

  case class Supplier(id: Int, name: String, street: String)

  object Suppliers extends Table[Supplier]("SUPPLIERS") {
    def id = column[Int]("SUP_ID", O.PrimaryKey, O.AutoInc)
    def name = column[String]("SUP_NAME")
    def street = column[String]("STREET")
    // Map a Supplier case class:
    def * = id ~ name ~ street <> (Supplier.tupled, Supplier.unapply)
    // Special mapping without the 'id' field:
    def forInsert = name ~ street <> (
      { case (name, street) => Supplier(-1, name, street) },
      { sup => (sup.name, sup.street) }
    )
  }

  Suppliers.forInsert.insert(mySupplier)

This is no longer necessary in 2.0. You can simply insert using the default
projection and Slick will skip the auto-incrementing ``id`` column:

.. includecode:: code/MigrationGuide.scala#insert2

If you really want to insert into an ``AutoInc`` field, you can use the new
methods ``forceInsert`` and ``forceInsertAll``.

Pre-compiled Updates
--------------------
Slick now supports pre-compilation of updates in the same manner like selects, see
:ref:`compiled-queries`.

.. index::
   pair: session; package

Database and Session Handling
-----------------------------

In Slick 1.0, the common JDBC-based ``Database`` and ``Session`` types, as well
as the ``Database`` factory object, could be found in the package
``scala.slick.session``. Since Slick 2.0 is no longer restricted to JDBC-based
databases, this package has been replaced by the new
:api:`scala.slick.backend.DatabaseComponent` (a.k.a. *backend*) hierarchy. If
you work at the :api:`scala.slick.driver.JdbcProfile` abstraction level, you
will always use a :api:`scala.slick.jdbc.JdbcBackend` from which you can import
the types that were previously found in ``scala.slick.session``. Note that
importing ``simple._`` from a driver will automatically bring these types into
scope.

Dynamically and Statically Scoped Sessions
------------------------------------------

Slick 2.0 still supports both, thread-local dynamic sessions and statically
scoped sessions, but the syntax has changed to make the recommended way of
using statically scoped sessions more concise. The old ``threadLocalSession``
is now called ``dynamicSession`` and the overloads of the associated session
handling methods ``withSession`` and ``withTransaction`` have been renamed to
``withDynSession`` and ``withDynTransaction`` respectively. If you used this
pattern in Slick 1.0::

  // --------------------- Slick 1.0 code -- does not compile in 2.0 ---------------------

  import scala.slick.session.Database.threadLocalSession

  myDB withSession {
    // use the implicit threadLocalSession here
  }

You have to change it for Slick 2.0 to:

.. includecode:: code/MigrationGuide.scala#dynsession

On the other hand, due to the overloaded methods, Slick 1.0 required
an explicit type annotation when using the statically scoped session:

.. includecode:: code/MigrationGuide.scala#session10

This is no longer necessary in 2.0:

.. includecode:: code/MigrationGuide.scala#session

Again, the recommended practice is NOT to use dynamic sessions.
If you are uncertain if you need them the answer is most probably no.
Static sessions are safer.

.. index:: MappedTypeMapper

Mapped Column Types
-------------------

Slick 1.0's ``MappedTypeMapper`` has been renamed to
:api:`MappedColumnType <scala.slick.driver.JdbcTypesComponent@MappedColumnType:JdbcDriver.MappedColumnTypeFactory>`.
Its basic form (using
:api:`MappedColumnType.base <scala.slick.profile.RelationalTypesComponent$MappedColumnTypeFactory@base[T,U]((T)⇒U,(U)⇒T)(ClassTag[T],RelationalDriver.BaseColumnType[U]):RelationalDriver.BaseColumnType[T]>`)
is now available at the :api:`scala.slick.profile.RelationalProfile` level (with
more advanced uses still requiring :api:`scala.slick.driver.JdbcProfile`). The
idiomatic use in Slick 1.0 was::

  // --------------------- Slick 1.0 code -- does not compile in 2.0 ---------------------

  case class MyID(value: Int)

  implicit val myIDTypeMapper =
    MappedTypeMapper.base[MyID, Int](_.value, new MyID(_))


This has changed to:

.. includecode:: code/MigrationGuide.scala#mappedcolumntype

If you need to map a simple wrapper type (as shown in this example), you can
now do that in an easier way by extending :api:`scala.slick.lifted.MappedTo`:

.. includecode:: code/MigrationGuide.scala#mappedto
