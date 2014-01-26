Migration Guide from Slick 1.0 to 2.0
=====================================

Slick 2.0 contains some improvements which are not source compatible with Slick
1.0. When migrating your application from 1.0 to 2.0, you will likely need to
perform changes in the following areas.

Code generation
-----------------

Instead of writing your table descriptions or plain SQL mappers by hand, in 2.0 you can
now automatically generate them from your database schema. The code-generator
is flexible enough to customize it's output to fit exactly what you need.
:doc:`More info on code generation <code-generation>`.

Table descriptions
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

In 1.0 the ``<>`` method for bidirectional mappings was overloaded for
different arities so you could directly pass a case class's ``apply`` method to
it::

  // --------------------- Slick 1.0 code -- does not compile in 2.0 ---------------------

  def * = id ~ name ~ street <> (Supplier _, Supplier.unapply)

This is no longer supported in 2.0. One of the reasons is that the overloading
lead to complicated error messages.
You now have to use a function with an appropriate tuple type.
If you map to a case class you can simply use ``.tupled`` on its
companion object:

.. includecode:: code/MigrationGuide.scala#mappedprojection

Pre-compiled updates
-----------------------------
Slick now supports pre-compilation of updates in the same manner like selects, see
:ref:`compiled-queries`.

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

Mapped Column Types
-------------------

Slick 1.0's ``MappedTypeMapper`` has been renamed to
:api:`MappedColumnType <scala.slick.driver.JdbcTypesComponent@MappedColumnType:JdbcDriver.MappedColumnTypeFactory>`.
Its basic form (using
:api:`MappedColumnType.base <scala.slick.profile.RelationalTypesComponent$MappedColumnTypeFactory@base[T,U]((T)⇒U,(U)⇒T)(ClassTag[T],(RelationalTypesComponent.this)#BaseColumnType[U]):(RelationalTypesComponent.this)#BaseColumnType[T]>`)
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
