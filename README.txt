ScalaQuery is a type-safe database query API for Scala.

It includes the following features:
- Session management based on JDBC Connections
- Type-safe queries based on a query monad and combinators
- Simple static and dynamic queries

The following database systems are directly supported for type-safe queries:
- PostgreSQL
- MySQL
- Microsoft SQL Server
- Microsoft Access
- H2
- HSQLDB/HyperSQL
- Derby/JavaDB
- SQLite
Accessing other database systems is possible, with a reduced feature set.

See <http://scalaquery.org/> for more information.
Licensing conditions (BSD-style) can be found in LICENSE.txt.

---
This fork adds support for TableOption-constraints and use them to give 
additional properties to MySQL-tables. For example (may not compile, but 
you get the idea):  

object TransferIds extends Table[Long]("table") {
  def id = column[Long]("id", O.NotNull, O.PrimaryKey)
  def * = id
  def engine = TableOption("ENGINE", "InnoDB")
  def collate = TableOption("COLLATE", "utf8_swedish_ci")
}

should produce a table with definition something like:
 
CREATE TABLE `table` (
  `id` long NOT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB COLLATE=utf8_swedish_ci;

