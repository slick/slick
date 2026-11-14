Database Migrations
===================
Slick itself does not have out-of-the-box support for database migrations,
but there are some third-party tools that work well with Slick. This page
provides a list of them.

slick-migration-api
-------------------
[slick-migration-api](https://github.com/nafg/slick-migration-api) is a
library for defining database migrations, for use with Slick. It supports
altering database schemas using a type-safe Scala DSL.

Flyway
------

[Flyway](https://flywaydb.org/) is a database migration tool that can
run SQL or Java against a database.

[sbt integration](https://github.com/sbt/flyway-sbt) is available. Other tools that run SQL migrations, such as
[Liquibase](https://www.liquibase.com/), work with Slick as well.

Flyway's Java-based migrations receive a connection that Flyway owns, often with a Flyway transaction open on it.
To write such a migration with Slick, see @ref:[Using a Connection Someone Else Owns](database.md#using-a-connection-someone-else-owns).
