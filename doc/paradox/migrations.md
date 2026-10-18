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
