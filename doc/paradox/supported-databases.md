# Supported Databases

* @extref[DB2](db2:)
* @extref[Derby](derby:) / @extref[JavaDB](javadb:)
* @extref[H2](h2:)
* @extref[HSQLDB](hsqldb:) (HyperSQL)
* Microsoft @extref[SQL Server](sql-server:)
* @extref[MySQL](mysql:)
* @extref[Oracle](oracle:)
* @extref[PostgreSQL](postgresql:)
* @extref[SQLite](sqlite:)

Other SQL databases can be accessed right away with a reduced feature set.
Writing a fully featured plugin for your own SQL-based backend can be achieved
with a reasonable amount of work. Support for other backends (like NoSQL) is
under development but not yet available.

The following capabilities are supported by the profiles. A checkmark means that a
capability is fully supported. In other cases it may be partially supported or
not at all. See the individual profile's API documentation for details.

@@include[ ](capabilities.md)
