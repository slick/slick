import org.scalatest._
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.time.{Seconds, Span}
import slick.jdbc.H2Profile.api._
import slick.jdbc.meta._

class TablesSuite extends FunSuite with BeforeAndAfter with ScalaFutures {
  implicit override val patienceConfig = PatienceConfig(timeout = Span(5, Seconds))

  val suppliers = TableQuery[Suppliers]
  val coffees = TableQuery[Coffees]
  
  var db: Database = _

  def createSchema() =
    db.run((suppliers.schema ++ coffees.schema).create).futureValue
  
  def insertSupplier(): Int =
    db.run(suppliers += (101, "Acme, Inc.", "99 Market Street", "Groundsville", "CA", "95199")).futureValue
  
  before { db = Database.forConfig("h2mem1") }
  
  test("Creating the Schema works") {
    createSchema()
    
    val tables = db.run(MTable.getTables).futureValue

    assert(tables.size == 2)
    assert(tables.count(_.name.name.equalsIgnoreCase("suppliers")) == 1)
    assert(tables.count(_.name.name.equalsIgnoreCase("coffees")) == 1)
  }

  test("Inserting a Supplier works") {
    createSchema()
    
    val insertCount = insertSupplier()
    assert(insertCount == 1)
  }
  
  test("Query Suppliers works") {
    createSchema()
    insertSupplier()
    val results = db.run(suppliers.result).futureValue
    assert(results.size == 1)
    assert(results.head._1 == 101)
  }
  
  after { db.close }
}
