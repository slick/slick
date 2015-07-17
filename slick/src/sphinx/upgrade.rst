.. index:: migration, 1.0, upgrading

Upgrade Guides
##############

.. index::
   pair: source; compatibility
   pair: binary; compatibility

Compatibility Policy
====================

Slick requires Scala 2.10 or 2.11. (For Scala 2.9 please use ScalaQuery_, the predecessor of Slick).

Slick version numbers consist of an epoch, a major and minor version, and possibly a qualifier
(for milestone, RC and SNAPSHOT versions).

For release versions (i.e. versions without a qualifier), backward binary compatibility is
guaranteed between releases with the same epoch and major version (e.g. you could use 2.1.2 as a
drop-in relacement for 2.1.0 but not for 2.0.0). :doc:`Slick Extensions <extensions>` requires at
least the same minor version of Slick (e.g. Slick Extensions 2.1.2 can be used with Slick 2.1.2 but
not with Slick 2.1.1). Binary compatibility is not preserved for `slick-codegen`, which is generally
used at compile-time.

We do not guarantee source compatibility but we try to preserve it within the same major release.
Upgrading to a new major release may require some changes to your sources. We generally deprecate
old features and keep them around for a full major release cycle (i.e. features which become
deprecated in 2.1.0 will not be removed before 2.2.0) but this is not possible for all kinds of
changes.

Release candidates have the same compatibility guarantees as the final versions to which they
lead. There are *no compatibility guarantees* whatsoever for milestones and snapshots.

Upgrade from 2.1 to 3.0
=======================

This section describes the changes that are needed when upgrading from Slick 2.1 to 3.0. If you are
currently using an older version of Slick, please see the older `Slick Manuals`_ for details on other
changes that may be required.

Package Structure
-----------------

Slick has moved from package ``scala.slick`` to ``slick``. A package object in ``scala.slick`` provides deprecated
aliases for many common types and values.

Database I/O Actions
--------------------

The ``simple`` and ``Implicits`` imports from drivers are deprecated and will be removed in Slick 3.1.
You should use ``api`` instead, which will give
you the same features, except for the old ``Invoker`` and ``Executor`` APIs for blocking execution of database calls.
These have been replaced by a new monadic database I/O actions API. See :doc:`Database I/O Actions <dbio>` for
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
requires Option and non-Option columns to be treated differently when creating an implicit ``Shape``. In this case
the evidence needs to be of type ``OptionTypedType[T]`` or ``BaseTypedType[T]``, respectively. If you want to abstract
over both, it may be more convenient to pass the required ``Shape`` as an implicit parameter and let it be instantiated
at the call site where the concrete type is known.

``Column[T]`` is still available as a deprecated alias for ``Rep[T]``. Due to the required implicit evidence, it
cannot provide complete backwards compatibility in all cases.

Closing Databases
-----------------

Since a ``Database`` instance can now have an associated connection pool and thread pool, it is
important to call ``shutdown`` or ``close`` when you are done using it, so that these pools can be
shut down properly. You should take care to do this when you migrate to the new action-based API.
As long as you exclusively use the deprecated synchronous API, it is not strictly necessary.

.. warning::
   Do not rely on the lazy initialization! Slick 3.1 will require ``Database`` objects to always be
   closed and may create connection and thread pool immediately.

Metadata API and Code Generator
-------------------------------

The JDBC metadata API in package ``slick.jdbc.meta`` has been switched to the new API, producing Actions instead
of Invokers. The code generator, which uses this API, has been completely rewritten for the asynchronous API. It still
supports the same functionality and the same concepts but any customization of the code generator will have to be
changed. See the code generator tests and the :doc:`code-generation` chapter for examples.

Inserting from Queries and Expressions
--------------------------------------

In Slick 2.0, soft inserts (where auto-incrementing columns are ignored) became the default for inserting raw values.
Inserting from another query or a computed expression still uses force-insert semantics (i.e. trying to insert even into
auto-incrementing columns, whether or not the database supports it). The new DBIO API properly reflects this by renaming
``insert(Query)`` to ``forceInsertQuery(Query)`` and ``insertExpr`` to ``forceInsertExpr``.

Default String Types
--------------------

The default type for ``String`` columns of unconstrained length in JdbcProfile has traditionally been ``VARCHAR(254)``.
Some drivers (like H2Driver) already changed it into an unconstrained string type. Slick 3.0 now also uses ``VARCHAR``
on PostgreSQL and ``TEXT`` on MySQL. The former should be harmless but MySQL's ``TEXT`` type is similar to ``CLOB`` and
has some limitations (e.g. no default values and no index without a prefix length). You can use an explicit
``O.Length(254)`` column option to go back to the previous behavior or change the default in the application.conf key
``slick.driver.MySQL.defaultStringType``.

JdbcDriver
----------

The ``JdbcDriver`` object has been deprecated. You should always use the correct driver for your database system.
