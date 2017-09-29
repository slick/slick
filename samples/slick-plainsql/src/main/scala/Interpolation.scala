import slick.jdbc.H2Profile.api._

import scala.concurrent.ExecutionContext.Implicits.global

trait Interpolation { this: PlainSQL.type =>

  def createCoffees: DBIO[Int] =
    sqlu"""create table coffees(
      name varchar not null,
      sup_id int not null,
      price double not null,
      sales int not null,
      total int not null,
      foreign key(sup_id) references suppliers(id))"""

  def createSuppliers: DBIO[Int] =
    sqlu"""create table suppliers(
      id int not null primary key,
      name varchar not null,
      street varchar not null,
      city varchar not null,
      state varchar not null,
      zip varchar not null)"""

  def insertSuppliers: DBIO[Unit] = DBIO.seq(
    // Insert some suppliers
    sqlu"insert into suppliers values(101, 'Acme, Inc.', '99 Market Street', 'Groundsville', 'CA', '95199')",
    sqlu"insert into suppliers values(49, 'Superior Coffee', '1 Party Place', 'Mendocino', 'CA', '95460')",
    sqlu"insert into suppliers values(150, 'The High Ground', '100 Coffee Lane', 'Meadows', 'CA', '93966')"
  )

  def insertCoffees: DBIO[Unit] = {
    def insert(c: Coffee): DBIO[Int] =
      sqlu"insert into coffees values (${c.name}, ${c.supID}, ${c.price}, ${c.sales}, ${c.total})"

    // Insert some coffees. The SQL statement is the same for all calls:
    // "insert into coffees values (?, ?, ?, ?, ?)"
    val inserts: Seq[DBIO[Int]] = Seq(
      Coffee("Colombian", 101, 7.99, 0, 0),
      Coffee("French_Roast", 49, 8.99, 0, 0),
      Coffee("Espresso", 150, 9.99, 0, 0),
      Coffee("Colombian_Decaf", 101, 8.99, 0, 0),
      Coffee("French_Roast_Decaf", 49, 9.99, 0, 0)
    ).map(insert)

    val combined: DBIO[Seq[Int]] = DBIO.sequence(inserts)
    combined.map(_.sum)
  }

  def printAll: DBIO[Unit] =
    // Iterate through all coffees and output them
    sql"select * from coffees".as[Coffee].map { cs =>
      println("Coffees:")
      for(c <- cs)
        println("* " + c.name + "\t" + c.supID + "\t" + c.price + "\t" + c.sales + "\t" + c.total)
    }

  def namesByPrice(price: Double): DBIO[Seq[(String, String)]] = sql"""
    select c.name, s.name
    from coffees c, suppliers s
    where c.price < $price and s.id = c.sup_id""".as[(String, String)]

  def supplierById(id: Int): DBIO[Seq[Supplier]] =
    sql"select * from suppliers where id = $id".as[Supplier]

  def printParameterized: DBIO[Unit] = {
    // Perform a join to retrieve coffee names and supplier names for
    // all coffees costing less than $9.00
    namesByPrice(9.0).flatMap { l2 =>
      println("Parameterized StaticQuery:")
      for (t <- l2)
        println("* " + t._1 + " supplied by " + t._2)
      supplierById(49).map(s => println(s"Supplier #49: $s"))
    }
  }

  def coffeeByName(name: String): DBIO[Option[Coffee]] = {
    val table = "coffees"
    sql"select * from #$table where name = $name".as[Coffee].headOption
  }

  def deleteCoffee(name: String): DBIO[Int] =
    sqlu"delete from coffees where name = $name"
}
