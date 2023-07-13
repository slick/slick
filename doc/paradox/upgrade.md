Upgrade Guides
==============

Compatibility Policy
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


Latest changes
--------------

See @ref:[the generated tables of incompatible changes](compat-report.md)

Upgrade from 3.4.x to 3.5.0
-----------------------

- `SynchronousDatabaseAction` has 5 type parameters instead of 4. See for instance
  https://github.com/tminglei/slick-pg/pull/651/files#diff-208b921209dd4b53867be1f55f1fa054d81f6473575ae4b7efd960a3d8c7a298L17

Upgrade from 3.3.x to 3.4.0
-----------------------

The full list of bug fixes and changes in version 3.4.0 is
[available on github](https://github.com/slick/slick/pulls?q=is%3Apr+is%3Amerged+milestone%3A3.4.0). 

### Dependency upgrades

Slick 3.4.0 updates the following upstream dependencies. If you use these libraries as part of your build, 
you should update your dependency versions to match:

| Dependency                                                                                         | Slick 3.3 depends on | Slick 3.4 depends on | Notes                                                                                                                                                                          |
|----------------------------------------------------------------------------------------------------|----------------------|----------------------|--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| [com.typesafe:config](https://github.com/lightbend/config)                                         | 1.3.2                | 1.4.2                | See the `config` library's [release notes](https://github.com/lightbend/config/blob/main/NEWS.md) for changes.                                                                 |
| [com.zaxxer:HikariCP](https://github.com/brettwooldridge/HikariCP)                                 | 3.2.0                | 4.0.3                | See the `HikariCP` [changelog](https://github.com/brettwooldridge/HikariCP/blob/dev/CHANGES) for changes. This only affects users who depend on the `slick-hikaricp` artifact. |
| [org.scala-lang.modules:scala-collection-compat](https://github.com/scala/scala-collection-compat) | 2.0.0                | 2.6.0                | Versions >= 2.0.0 should be binary-compatible, so no issues are expected. This only affects users building with Scala 2.11 or 2.12.                                            |

### AsyncExecutor defaults

`AsyncExecutor` wraps the thread pool that Slick uses to run blocking I/O (such as database queries). The
previous default constructor arguments could potentially cause deadlocks in some cases, so Slick 3.4 changes these 
to safer defaults. In most cases this should be a drop-in replacement but users are encourage to test performance
carefully after upgrading. See issue [#1938](https://github.com/slick/slick/issues/1938) for details.  

### PostgreSQL `java.time` mappings

The following bug fixes are highlighted here because they change the default mapping behaviour for some `java.time`
columns in PostgreSQL:

- `java.time.Instant.MIN` and `java.time.Instant.MAX` are now correctly mapped to `-infinity` and `infinity`
  respectively in PostgreSQL profile ([#2237](https://github.com/slick/slick/issues/2237))
- changes to handling of timezone part of `java.time.Instant` in PostgreSQL profile
  ([#2005](https://github.com/slick/slick/issues/2005))

Upgrade from 3.4 to 3.5
-----------------------

### ResultConverter

``ResultConverter`` is an internal interface for converting between values handled in JDBC and values handled in
application code (case classes, etc.). While you most likely aren't using it directly, you may need to call methods
of this interface if you are override behavior of a profile or writing a new one.

In order to support a single insert statement with multiple rows, the `set` method now takes an `offset` parameter.
If you have extended a profile and used the `set` method of this interface, you may need to specify 0 for the offset
(unless the number of variable placeholders in the SQL dynamically changes).

Upgrade from 3.2 to 3.3
-----------------------

### Create / Drop If Not Exists

There is no major changes in the API except for the addition of `createIfNotExists` and `dropIfExistsPhase`.
This has only impact on database profile developers. Regular users are not impacted by it.

In other to support `createIfNotExists` and `dropIfExistsPhase`, the following changes were made:

* slick.jdbc.JdbcStatementBuilderComponent#TableDDLBuilder.createTable receives not a `checkNotExists: Boolean` as
  argument
* slick.jdbc.JdbcStatementBuilderComponent#TableDDLBuilder.dropTable receives not a `ifExists: Boolean` as argument
* slick.sql.SqlProfile#DDL.apply has two more arguments `createIfNotExists: Iterable[String]` and 
  `dropIfExists: Iterable[String]`

### Support for `java.time` columns

Slick 3.3.0 profiles now supports `java.time` types as columns (for example, in `Table` `column` definitions).

If you already have [custom mappings](https://scala-slick.org/doc/3.3.0/userdefined.html#scalar-types) for these types,
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

In the tables that follow there are many similarities between databases. However, not all databases can directly map the
various `java.time` semantics. In these cases, a `VARCHAR` may have been used to store a formatted time representation.
For more details see the [description of Slick's approach](https://scala-slick.org/doc/3.3.0/datetimetypes.html) to
these mappings.

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
(like @scaladoc[JdbcProfile](slick.jdbc.JdbcProfile)) or concrete (like @scaladoc[H2Profile](slick.jdbc.H2Profile)). The old names are still
available as deprecated forwarders.

The renaming also affects configuration parameters (for example, for  @ref:[Databaseconfig](database.md#databaseconfig)).
The old parameter names are still supported but their use will generate warnings at runtime.

### Slick Extensions

The `Slick Extensions` package with additional profiles for Oracle, DB2 and SQL Server does not exist
anymore for Slick 3.2. These profiles have been added to the core Slick package:

* @scaladoc[OracleProfile](slick.jdbc.OracleProfile)
* @scaladoc[DB2Profile](slick.jdbc.DB2Profile)
* @scaladoc[SQLServerProfile](slick.jdbc.SQLServerProfile)

### Database Action Scheduling

When configuring a  @ref:[database connection](database.md) with a connection pool with a limited maximum size, you
should always specify the correct size limit in Slick. This is necessary to prevent deadlocks when using transactions
(or other pinned sessions). The configuration is done automatically when using Slick's built-in HikariCP support.
You only need to configure it manually if you use a connection pool through a JDBC `DataSource` or JNDI name.


@@@ index
  * @ref:[.](compat-report.md)
@@@
