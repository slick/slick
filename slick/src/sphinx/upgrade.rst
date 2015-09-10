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

Upgrade from 3.0 to 3.1
=======================

This section describes the changes that are needed when upgrading from Slick 3.0 to 3.1. If you are
currently using an older version of Slick, please see the older `Slick Manuals`_ for details on other
changes that may be required.

Deprecations
------------

Most deprecated features from 3.0, including the old ``Invoker`` and ``Executor`` APIs and the package
aliases for ``scala.slick`` were removed.

HikariCP
--------

The HikariCP_ support for Slick was factored out into its own module with a non-optional dependency
on HikariCP itself. This makes it easier to use the correct version of HikariCP (which does not have
a well-defined binary compatibility policy) with Slick. See the section on :ref:`dependencies <dependencies>`
for more information.

Due to packaging constraints imposed by OSGi, :hikaricpapi:`slick.jdbc.hikaricp.HikariCPJdbcDataSource`
was moved from package ``slick.jdbc`` to ``slick.jdbc.hikaricp``.

Counting Option columns
-----------------------

Counting any multi-column collection with `.length` now ignores nullability of the columns. The previous
approach of picking a random column led to inconsistent results. This is particularly relevant when you
try to count one side of an outer join. Up to Slick 3.0 the goal (although not achieved in all cases due
to a design problem) was not to include non-matching rows in the total (equivalent to counting the
discriminator column only). This does not make sense anymore for the new outer join operators (introduced
in 3.0) with correct `Option` types. The new semantics are identical to those of Scala collections.
Semantics for counts of single columns remain unchanged.

Default String type on MySQL
----------------------------

Slick 3.0 changed the default string type for MySQL to `TEXT`, which is not allowed for primary keys
and columns with default values. In these cases we now fall back to the old `VARCHAR(254)` type which
was used up to Slick 2.1. Like in 3.0 you can change this default by setting the application.conf key
`slick.driver.MySQL.defaultStringType`.

Default String type on SQL Server
---------------------------------

SQLServerDriver (part of :doc:`Slick Extensions <extensions>`) used `VARCHAR(MAX)` as the default type
for strings without a size limit in 3.0. This type cannot be used for primary keys, so we now use
`VARCHAR(254)` instead if a column has the `PrimaryKey` option set. Like for MySQL (see previous
paragraph), the default type can be overridden by setting `slick.driver.SQLServer.defaultStringType`
in application.conf.
