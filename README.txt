Project SLICK (Scala Language Integrated Connection Kit) aims at the
development of a complete, modern, convenient, efficient and safe data query
and access framework for the Scala programming language that operates on the
Java platform. Ordinary Scala code can be used to query different database
back-ends. Queries can be processed and validated at compile time, even in
user-provided back-ends.

It includes the following features:
- Query language with type-safe queries based on a query monad and combinators
- Lifting plain Scala code to database queries
- Simple execution of raw SQL queries
- Session management based on JDBC Connections

The following database systems are directly supported for type-safe queries:
- H2

Other previously supported back-ends (in ScalaQuery 0.9) which have not yet
been fully integrated into SLICK 0.10:
- PostgreSQL
- MySQL
- Microsoft SQL Server
- Microsoft Access
- HSQLDB/HyperSQL
- Derby/JavaDB
- SQLite

Accessing other database systems is possible, with a reduced feature set.

See <https://github.com/typesafehub/slick/wiki> for more information.
Licensing conditions (BSD-style) can be found in LICENSE.txt.
