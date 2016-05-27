Database Migrations
===========================
Slick itself does not have out-of-the-box support for database migrations,
but there are some third-party tools that work well with Slick. This page
provides a list of them.

Scala Forklift
---------------------------
`Scala Forklift <https://github.com/lastland/scala-forklift>`_ is a
type-safe data migration tool with great support for Slick. Users can
define their database migrations using either type-safe Slick queries
or plain SQL queries. Scala Forklift will automatically apply the
unapplied user-defined database migrations in the right order, and
manage the Scala code corresponding to the database schemas using
the Slick code generator.

An example can be found
`here <https://github.com/lastland/scala-forklift/tree/develop/example>`_.

An example of using Play, Slick, and Forklift can be found
`here <https://github.com/lastland/play-slick-forklift-example>`_.

slick-migration-api
--------------------------
`slick-migration-api <https://github.com/nafg/slick-migration-api>`_ is a
library for defining database migrations, for use with Slick. It supports
altering database schemas using a type-safe Scala DSL. Unfortunately, it
does not support Slick of version 3.0 or later for the time being.
