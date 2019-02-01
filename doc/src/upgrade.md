Upgrade Guides {index="migration; 1.0; upgrading"}
==============

Compatibility Policy {index="source,compatibility; compatibility,source; binary,compatibility; compatibility,binary"}
--------------------

Slick 3.3.0 requires Scala 2.11 or 2.12 and Java 8.

Slick version numbers consist of an epoch, a major and minor version, and possibly a qualifier
(for milestone, RC and SNAPSHOT versions).

For release versions (i.e. versions without a qualifier), backward binary compatibility is
guaranteed between releases with the same epoch and major version (e.g. you could use 2.1.2 as a
drop-in replacement for 2.1.0 but not for 2.0.0). Binary compatibility is not preserved for
`slick-codegen`, which is generally used at compile-time.

We do not guarantee source compatibility but we try to preserve it within the same major release.
Upgrading to a new major release may require some changes to your sources. We generally deprecate
old features and keep them around for a full major release cycle (i.e. features which become
deprecated in 2.1.0 will not be removed before 2.2.0) but this is not possible for all kinds of
changes.

Release candidates have the same compatibility guarantees as the final versions to which they
lead. There are *no compatibility guarantees* whatsoever for milestones and snapshots.

Upgrade from 3.2 to 3.3
-----------------------

### Create / Drop If Not Exists

There is no major changes in the API except for the addition of `createIfNotExists` and `dropIfExistsPhase`. This has only impact on database profile developers. Regular users are not impacted by it.

In other to support `createIfNotExists` and `dropIfExistsPhase`, the following changes were made:

* slick.jdbc.JdbcStatementBuilderComponent#TableDDLBuilder.createTable receives not a `checkNotExists: Boolean` as argument
* slick.jdbc.JdbcStatementBuilderComponent#TableDDLBuilder.dropTable receives not a `ifExists: Boolean` as argument
* slick.sql.SqlProfile#DDL.apply has two more arguments `createIfNotExists: Iterable[String]` and `dropIfExists: Iterable[String]`


Upgrade from 3.1 to 3.2
-----------------------

This section describes the changes that are needed when upgrading from Slick 3.1 to 3.2. If you are
currently using an older version of Slick, please see the older [Slick Manuals] for details on other
changes that may be required.

### Profiles vs Drivers

Slick's `driver` concept has been renamed to `profile` to end the confusion over Slick drivers vs JDBC drivers.
All references to `drivers` from now on refer to JDBC drivers. Slick `profiles` can be either abstract
(like <api:slick.jdbc.JdbcProfile>) or concrete (like <api:slick.jdbc.H2Profile>). The old names are still
available as deprecated forwarders.

The renaming also affects configuration parameters (for example, for [Databaseconfig](database.md#databaseconfig)).
The old parameter names are still supported but their use will generate warnings at runtime.

### Slick Extensions

The `Slick Extensions` package with additional profiles for Oracle, DB2 and SQL Server does not exist
anymore for Slick 3.2. These profiles have been added to the core Slick package:

* <api:slick.jdbc.OracleProfile>
* <api:slick.jdbc.DB2Profile>
* <api:slick.jdbc.SQLServerProfile>

### Database Action Scheduling

When configuring a [database connection](database.md) with a connection pool with a limited maximum size, you
should always specify the correct size limit in Slick. This is necessary to prevent deadlocks when using transactions
(or other pinned sessions). The configuration is done automatically when using Slick's built-in HikariCP support.
You only need to configure it manually if you use a connection pool through a JDBC `DataSource` or JNDI name.
