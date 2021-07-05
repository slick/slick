Upgrade Guides {index="migration; 1.0; upgrading"}
==============

Compatibility Policy {index="source,compatibility; compatibility,source; binary,compatibility; compatibility,binary"}
--------------------

Slick 3.4.0 requires 2.12 or 2.13 and Java 8, 11 or higher.

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

### Support for `java.time` columns

Slick 3.3.0 profiles now supports `java.time` types as columns (for example, in `Table` `column` definitions).

If you already have [custom mappings](http://slick.lightbend.com/doc/3.3.0/userdefined.html#scalar-types) for these types,
please review the formats in the tables below.
These formats are the built-in Slick mappings,
and they are the ones applied when you upgrade to Slick 3.3.0.
That is, they take precedence over any `MappedColumnType` you may have defined for the `java.time` types.

The types are: `Instant`, `LocalDate`, `LocalTime`, `LocalDateTime`, `OffsetTime`, `OffsetDateTime`, and `ZonedDateTime`.

If you need to customise these formats,
you can by extending a `Profile` and overriding the appropriate methods.
For an example of this see: <https://github.com/d6y/instant-etc/blob/master/src/main/scala/main.scala#L9-L45>.
Also of use will be an example of a full mapping, such as: <https://github.com/slick/slick/blob/v3.3.0/slick/src/main/scala/slick/jdbc/JdbcTypesComponent.scala#L187-L365>.

If you wish to upgrade your application to use the new formats,
you may need to migrate your database columns to match the Slick column formats.

In the tables that follow there are many similarities between databases.
However, not all databases can directly map the various `java.time` semantics.
In these cases, a `VARCHAR` may have been used to store a formatted time representation.
For more details see the [description of Slick's approach](http://slick.lightbend.com/doc/3.3.0/datetimetypes.html) to these mappings.

#### `slick.jdbc.DB2Profile`

| Java Type | SQL Type | Example SQL Literal |
|-----------|----------|---------------------|
| `java.time.Instant` | `VARCHAR(254)` | `'2019-02-03T18:20:28.660Z'` |
| `java.time.LocalDate` | `DATE` | `'2019-02-03'` |
| `java.time.LocalTime` | `VARCHAR(254)` | `'18:20:28.661'` |
| `java.time.LocalDateTime` | `TIMESTAMP` | `'2019-02-03 18:20:28.661'` |
| `java.time.OffsetTime` | `VARCHAR(254)` | `'18:20:28.661Z'` |
| `java.time.OffsetDateTime` | `VARCHAR(254)` | `'2019-02-03T18:20:28.661Z'` |
| `java.time.ZonedDateTime` | `VARCHAR(254)` | `'2019-02-03T18:20:28.661Z[Europe/London]'` |


#### `slick.jdbc.DerbyProfile`

| Java Type | SQL Type | Example SQL Literal |
|-----------|----------|---------------------|
| `java.time.Instant` | `VARCHAR(254)` | `'2019-02-03T18:20:28.660Z'` |
| `java.time.LocalDate` | `DATE` | `'2019-02-03'` |
| `java.time.LocalTime` | `VARCHAR(254)` | `'18:20:28.661'` |
| `java.time.LocalDateTime` | `TIMESTAMP` | `'2019-02-03 18:20:28.661'` |
| `java.time.OffsetTime` | `VARCHAR(254)` | `'18:20:28.661Z'` |
| `java.time.OffsetDateTime` | `VARCHAR(254)` | `'2019-02-03T18:20:28.661Z'` |
| `java.time.ZonedDateTime` | `VARCHAR(254)` | `'2019-02-03T18:20:28.661Z[Europe/London]'` |


#### `slick.jdbc.H2Profile`

| Java Type | SQL Type | Example SQL Literal |
|-----------|----------|---------------------|
| `java.time.Instant` | `TIMESTAMP(9) WITH TIME ZONE` | `'2019-02-03T18:20:28.660Z'` |
| `java.time.LocalDate` | `DATE` | `'2019-02-03'` |
| `java.time.LocalTime` | `VARCHAR` | `'18:20:28.661'` |
| `java.time.LocalDateTime` | `TIMESTAMP` | `'2019-02-03 18:20:28.661'` |
| `java.time.OffsetTime` | `VARCHAR` | `'18:20:28.661Z'` |
| `java.time.OffsetDateTime` | `VARCHAR` | `'2019-02-03T18:20:28.661Z'` |
| `java.time.ZonedDateTime` | `VARCHAR` | `'2019-02-03T18:20:28.661Z[Europe/London]'` |


#### `slick.jdbc.HsqldbProfile`

| Java Type | SQL Type | Example SQL Literal |
|-----------|----------|---------------------|
| `java.time.Instant` | `TIMESTAMP(9) WITH TIME ZONE` | `'2019-02-03 18:20:28.66'` |
| `java.time.LocalDate` | `DATE` | `'2019-02-03'` |
| `java.time.LocalTime` | `TIME(3)` | `'18:20:28.661'` |
| `java.time.LocalDateTime` | `TIMESTAMP` | `'2019-02-03 18:20:28.661'` |
| `java.time.OffsetTime` | `TIME(9) WITH TIME ZONE` | `'18:20:28.661+0:00'` |
| `java.time.OffsetDateTime` | `TIMESTAMP(9) WITH TIME ZONE` | `'2019-02-03 18:20:28.661+0:00'` |
| `java.time.ZonedDateTime` | `LONGVARCHAR` | `'2019-02-03T18:20:28.661Z[Europe/London]'` |


#### `slick.jdbc.SQLServerProfile`

| Java Type | SQL Type | Example SQL Literal |
|-----------|----------|---------------------|
| `java.time.Instant` | `DATETIMEOFFSET(6)` | `(convert(datetimeoffset(6), '2019-02-03 18:20:28.66 '))` |
| `java.time.LocalDate` | `DATE` | `'2019-02-03'` |
| `java.time.LocalTime` | `TIME(6)` | `(convert(time(6), '18:20:28.661'))` |
| `java.time.LocalDateTime` | `DATETIME2(6)` | `'2019-02-03 18:20:28.661'` |
| `java.time.OffsetTime` | `VARCHAR(MAX)` | `'18:20:28.661Z'` |
| `java.time.OffsetDateTime` | `DATETIMEOFFSET(6)` | `(convert(datetimeoffset(6), '2019-02-03 18:20:28.661 '))` |
| `java.time.ZonedDateTime` | `VARCHAR(MAX)` | `'2019-02-03T18:20:28.661Z[Europe/London]'` |


#### `slick.jdbc.MySQLProfile`

| Java Type | SQL Type | Example SQL Literal |
|-----------|----------|---------------------|
| `java.time.Instant` | `TEXT` | `'2019-02-03T18:20:28.660Z'` |
| `java.time.LocalDate` | `DATE` | `'2019-02-03'` |
| `java.time.LocalTime` | `TEXT` | `'18:20:28.661'` |
| `java.time.LocalDateTime` | `TEXT` | `'2019-02-03T18:20:28.661'` |
| `java.time.OffsetTime` | `TEXT` | `'18:20:28.661Z'` |
| `java.time.OffsetDateTime` | `TEXT` | `'2019-02-03T18:20:28.661Z'` |
| `java.time.ZonedDateTime` | `TEXT` | `'2019-02-03T18:20:28.661Z[Europe/London]'` |


#### `slick.jdbc.OracleProfile`

| Java Type | SQL Type | Example SQL Literal |
|-----------|----------|---------------------|
| `java.time.Instant` | `TIMESTAMP(9) WITH TIME ZONE` | `TO_TIMESTAMP_TZ('2019-02-03 18:20:28.660 +00', 'YYYY-MM-DD HH24:MI:SS.FF3 TZH')` |
| `java.time.LocalDate` | `DATE` | `TO_DATE('2019-02-03', 'SYYYY-MM-DD')` |
| `java.time.LocalTime` | `VARCHAR2(254)` | `'18:20:28.661'` |
| `java.time.LocalDateTime` | `TIMESTAMP` | `TO_TIMESTAMP('2019-02-03 18:20:28.661', 'YYYY-MM-DD HH24:MI:SS.FF3')` |
| `java.time.OffsetTime` | `TIMESTAMP(6) WITH TIME ZONE` | `TO_TIMESTAMP_TZ('1970-01-01 18:20:28.661 +0000', 'YYYY-MM-DD HH24:MI:SS.FF3 TZH:TZM')` |
| `java.time.OffsetDateTime` | `TIMESTAMP(6) WITH TIME ZONE` | `TO_TIMESTAMP_TZ('2019-02-03 18:20:28.661 +0000', 'YYYY-MM-DD HH24:MI:SS.FF3 TZH:TZM')` |
| `java.time.ZonedDateTime` | `TIMESTAMP(6) WITH TIME ZONE` | `TO_TIMESTAMP_TZ('2019-02-03 18:20:28.661 Europe/London', 'YYYY-MM-DD HH24:MI:SS.FF3 TZR')` |


#### `slick.jdbc.PostgresProfile`

| Java Type | SQL Type | Example SQL Literal |
|-----------|----------|---------------------|
| `java.time.Instant` | `TIMESTAMP` | `'2019-02-03 18:20:28.66'` |
| `java.time.LocalDate` | `DATE` | `'2019-02-03'` |
| `java.time.LocalTime` | `TIME` | `'18:20:28.661'` |
| `java.time.LocalDateTime` | `TIMESTAMP` | `'2019-02-03 18:20:28.661'` |
| `java.time.OffsetTime` | `TIMETZ` | `'18:20:28.661Z'` |
| `java.time.OffsetDateTime` | `VARCHAR` | `'2019-02-03T18:20:28.661Z'` |
| `java.time.ZonedDateTime` | `VARCHAR` | `'2019-02-03T18:20:28.661Z[Europe/London]'` |


#### `slick.jdbc.SQLiteProfile`

| Java Type | SQL Type | Example SQL Literal |
|-----------|----------|---------------------|
| `java.time.Instant` | `TIMESTAMP` | `1549218028660` |
| `java.time.LocalDate` | `DATE` | `1549152000000` |
| `java.time.LocalTime` | `VARCHAR(254)` | `'18:20:28.661'` |
| `java.time.LocalDateTime` | `TIMESTAMP` | `1549218028661` |
| `java.time.OffsetTime` | `VARCHAR(254)` | `'18:20:28.661Z'` |
| `java.time.OffsetDateTime` | `VARCHAR(254)` | `'2019-02-03T18:20:28.661Z'` |
| `java.time.ZonedDateTime` | `VARCHAR(254)` | `'2019-02-03T18:20:28.661Z[Europe/London]'` |


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
