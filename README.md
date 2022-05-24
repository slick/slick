# Slick

[![Maven](https://img.shields.io/maven-central/v/com.typesafe.slick/slick_2.13.svg)](http://mvnrepository.com/artifact/com.typesafe.slick/slick_2.13) [![Gitter](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/slick/slick?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge)

Slick is a functional database library for Scala.

It allows you to work with relational databases almost as if you were using 
Scala collections, while at the same time giving you full control over when a 
database access happens and what data is transferred. By writing your queries 
in Scala you can benefit from the static type checking, compile-time safety, and 
compositionality of Scala, while retaining the ability to drop down to raw SQL 
where needed for custom or advanced database features.

Slick also features an advanced query compiler which can generate SQL for a variety
of different database engines from the same Scala code, allowing you to focus on
application logic without worrying about database-specific syntax and quirks.

## Resources

- Full documentation, including Scaladocs and more complex examples, can be 
found on the [Slick website](https://scala-slick.org)

- We have an active [gitter channel](https://gitter.im/slick/slick) where you
can ask for help

- Think you've found a bug? Have an idea for a new feature? Please raise it in
our [issue tracker](https://github.com/slick/slick/issues) here on github

- Our friends at [`underscore.io`](https://underscore.io/) have written "Essential 
Slick", an excellent guide to using slick from first principles, which is now 
available [as a free download](https://underscore.io/books/essential-slick/)

## Example

As a simple example we will create a Scala object `Coffee`, and a table to store 
instances of this object in the database:

```scala
// First declare our Scala object
final case class Coffee(name: String, price: Double)

// Next define how Slick maps from a database table to Scala objects
class Coffees(tag: Tag) extends Table[Coffee](tag, "COFFEES") {
  def name  = column[String]("NAME")
  def price = column[Double]("PRICE")
  def * = (name, price).mapTo[Coffee]
}

// The `TableQuery` object gives us access to Slick's rich query API
val coffees = TableQuery[Coffees]

// Inserting is done by appending to our query object
// as if it were a regular Scala collection
// SQL: insert into COFFEES (NAME, PRICE) values ('Latte', 2.50)
coffees += Coffee("Latte", 2.50)

// Fetching data is also done using the query object
// SQL: select NAME from COFFEES
coffees.map(_.name)

// More complex queries can be chained together
// SQL: select NAME, PRICE from COFFEES where PRICE < 10.0 order by NAME
coffees.filter(_.price < 10.0).sortBy(_.name)
```

## Database support

The following databases are directly supported by Slick, and are currently covered
by a large suite of automated tests to ensure compatibility:

|Database|JDBC Driver|
|--------|-----------|
|DB2 10.5|[db2jcc4:4.19.20](http://www-01.ibm.com/support/docview.wss?uid=swg21363866)|
|Derby/JavaDB|derby:10.14.2.0|
|H2|com.h2database.h2:1.4.199|
|HSQLDB/HyperSQL|hsqldb:2.4.1|
|MySQL|mysql-connector-java:8.0.16|
|Oracle 11g|[ojdbc7:12.1.0.2](http://www.oracle.com/technetwork/database/features/jdbc/index-091264.html)|
|PostgreSQL|postgresql:42.2.5|
|SQLite|sqlite-jdbc:3.27.2.1|
|SQLServer 2008, 2012, 2014, 2017|[jtds:1.3.1](http://sourceforge.net/projects/jtds/files/jtds/) and [msjdbc:7.2.2](https://docs.microsoft.com/en-us/sql/connect/jdbc/download-microsoft-jdbc-driver-for-sql-server?view=sql-server-2017)|

Accessing other database systems is possible, although possibly with a reduced feature 
set.

## Contributing

Slick is community-maintained: pull requests are very welcome, and we
ask that all contributors abide by the [Lightbend Community Code of Conduct](https://www.lightbend.com/conduct).

Lightbend staff (such as @SethTisue) may be able to assist with
administrative issues.
