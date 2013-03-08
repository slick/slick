package scala.slick.typeProviders

import com.typesafe.config.ConfigFactory
import scala.slick.lifted.ForeignKeyAction

/**
 * A simple example that uses statically typed queries against an in-memory
 * H2 database. The example data comes from Oracle's JDBC tutorial at
 * http://download.oracle.com/javase/tutorial/jdbc/basics/tables.html.
 */
object Main1 {

  // Use H2Driver to connect to an H2 database
  //import scala.slick.driver.H2Driver.simple._
  //import scala.slick.driver.H2Driver.simple.{ Table => Tab }
  //import _root_.scala.slick.driver.HsqldbDriver.simple._
  //import _root_.scala.slick.driver.HsqldbDriver.simple.{ Table => Tab }
  import _root_.scala.slick.driver.SQLiteDriver.simple._
  import _root_.scala.slick.driver.SQLiteDriver.simple.{ Table => Tab }
  import Database.threadLocalSession
  import _root_.scala.slick.jdbc.meta.MTable
  import _root_.scala.slick.jdbc.meta.MColumn

  case class Supplier(id: Int, name: String, street: String, city: String, state: String, zip: String)

  // Definition of the SUPPLIERS table
  object Suppliers extends Tab[Supplier]("SUPPLIERS") {
    def id = column[Int]("SUP_ID", O.PrimaryKey) // This is the primary key column
    def name = column[String]("SUP_NAME")
    def street = column[String]("STREET")
    def city = column[String]("CITY")
    def state = column[String]("STATE")
    def zip = column[String]("ZIP")
    // Every table needs a * projection with the same type as the table's type parameter
    def * = id ~ name ~ street ~ city ~ state ~ zip <> (Supplier, Supplier.unapply _)
  }

  // Definition of the COFFEES table
  object Coffees extends Tab[(String, Int, Double, Int, Int)]("COFFEES") {
    def name = column[String]("COF_NAME", O.PrimaryKey)
    def supID = column[Int]("SUP_ID")
    def price = column[Double]("PRICE")
    def sales = column[Int]("SALES")
    def total = column[Int]("TOTAL")
    def * = name ~ supID ~ price ~ sales ~ total
    // A reified foreign key relation that can be navigated to create a join
    def supplier = foreignKey("SUP_FK", supID, Suppliers)(_.id)
  }

  //  val d1 = Database.forURL("jdbc:h2:coffees/coffees", driver = "org.h2.Driver", user = "sa", password = "")
  //  val d2 = Database.forURL("jdbc:h2:mem:test1", driver = "org.h2.Driver")
  //  val d3 = Database.forURL("jdbc:hsqldb:suppliers/supp;shutdown=true", driver = "org.hsqldb.jdbcDriver", user = "", password = "")
  //  val d3 = Database.forURL("jdbc:h2:coffees/supp", driver = "org.h2.Driver", user = "", password = "")
  val d3 = Database.forURL("jdbc:sqlite:coffees/sqlite-supp.db", driver = "org.sqlite.JDBC", user = "", password = "")

  // Connect to the database and execute the following block within a session
  d3 withSession {
    // The session is never named explicitly. It is bound to the current
    // thread as the threadLocalSession that we imported
    //	val tables = MTable.getTables(None, None, Some("%"), Some(Seq("TABLE"))).list 
    val tables = MTable.getTables(None, None, None, None).list
    tables foreach (t => {
      println(t.name.name)
      println(t.getColumns.list.map(c => c.typeName).mkString("\n* "))
    })
    // Create the tables, including primary and foreign keys
    //    (Suppliers.ddl ++ Coffees.ddl).create

    // Insert some suppliers
    //    val s1 = Supplier(101, "Acme, Inc.", "99 Market Street", "Groundsville", "CA", "95199")
    //    val s2 = Supplier(49, "Superior Coffee", "1 Party Place", "Mendocino", "CA", "95460")
    //    val s3 = Supplier(150, "The High Ground", "100 Coffee Lane", "Meadows", "CA", "93966")
    //    Suppliers.insert(s1)
    //    Suppliers.insert(s2)
    //    Suppliers.insert(s3)

    println("Suppliers:")
    Query(Suppliers) foreach { sup =>
      println(sup)
    }
  }
  //  val conn = d3.createSession.conn
  //  println(Jdbc.tables(conn))
  //  println(Jdbc.list(conn, "show columns from " + "suppliers"))
}

object Main2 {
  import scala.reflect.runtime.universe._

  val m = runtimeMirror(getClass.getClassLoader)
  val module = m.staticModule("org.hsqldb.jdbcDriver")

  //  val expr = reify {
  //	  import scala.reflect.runtime.universe._
  //	  
  //	  ;
  //  }
  //  val s = showRaw(expr)
  //  println(s)
  //  println(expr.actualType)
}

object Main3 extends App{

  // Use H2Driver to connect to an H2 database
  import scala.slick.driver.H2Driver.simple._
  import scala.slick.driver.H2Driver.simple.{ Table => Tab }
  import Database.threadLocalSession
  import _root_.scala.slick.jdbc.meta.MTable
  import _root_.scala.slick.jdbc.meta.MColumn

  object A extends Table[(Int, Int, String)]("a") {
      def k1 = column[Int]("k1")
      def k2 = column[Int]("k2")
      def s = column[String]("s")
      def * = k1 ~ k2 ~ s
//      def bFK = foreignKey("b_fk", (k1, k2), B)(b => (b.f1, b.f2), onDelete = ForeignKeyAction.Cascade)
      def bFK = foreignKey("b_fk", k1, B)(b => b.f1, onDelete = ForeignKeyAction.Cascade)
    }

    object B extends Table[(Int, Int, String)]("b") {
      def f1 = column[Int]("f1")
      def f2 = column[Int]("f2")
      def s = column[String]("s")
      def * = f1 ~ f2 ~ s
//      def bIdx1 = index("b_idx1", (f1, f2), unique = true)
    }

//  val d2 = Database.forURL("jdbc:h2:mem:test1;INIT=runscript from 'create.sql'\\;runscript from 'populate.sql'", driver = "org.h2.Driver")
  val d2 = Database.forURL("jdbc:h2:mem:test1", driver = "org.h2.Driver")

  // Connect to the database and execute the following block within a session
  d2 withSession {
    (A.ddl ++ B.ddl).create
    println((A.ddl ++ B.ddl).createStatements.mkString("\n"))
    //	val tables = MTable.getTables(None, None, Some("%"), Some(Seq("TABLE"))).list 
        val tables = MTable.getTables(Some(""), Some(""), None, None).list
        tables foreach (t => {
          println(t)
          println(t.name.name)
          println(t.getColumns.list.map(c => c.toString()).mkString("\n"))
          println(t.getPrimaryKeys.list.mkString("\n"))
          println("=====")
          val fks = t.getImportedKeys.list
          val grouped = fks.groupBy(x => (x.pkTable.name, x.fkTable.name)).mapValues(_.map(x => (x.pkColumn, x.fkColumn)))
          println(grouped.mkString("\n"))
          println("=====")
          println(t.getImportedKeys.list.mkString("<Imp>\n\t", "\n\t", "\n</Imp>"))
          println(t.getExportedKeys.list.mkString("\n"))
          println(t.getIndexInfo(true, false).list)
        })
  }
}

object Main4  {

  val conf = com.typesafe.config.ConfigFactory.load("type-providers-h2")
  object ConfigHelper {
    def get(key: String) = {
      try {
        conf.getString(key)
      } catch {
        case _: Throwable => ""
      }

    }
  }
  println(conf.getString("username"))
}

object Main5 {
  import scala.slick.driver.H2Driver.simple._
  import scala.slick.driver.H2Driver.simple.{ Table => Tab }
  import scala.reflect.runtime.universe._
  case class Supplier(id: Int, name: String, flag: Int)

  val expr = reify {

    // Definition of the SUPPLIERS table
    object Suppliers extends Table[Supplier]("SUPPLIERS") {
      def id = column[Int]("SUP_ID")
      def name = column[String]("SUP_NAME")
      def flag = column[Int]("SUP_FLAG")
      def * = id ~ name ~ flag <> (Supplier, Supplier.unapply _)
//      def * = null
      def pk = primaryKey("pk_a", (id, name))
    }

  }
  val s = showRaw(expr)
  println(s)
}

object Main6 {
  import scala.slick.driver.H2Driver.simple._
  import scala.slick.driver.H2Driver.simple.{ Table => Tab }
  import scala.reflect.runtime.universe._
  case class Supplier(id: Int, name: String, flag: Int)

  val expr = reify {
    
    object A extends Table[(Int, Int, String)]("a") {
      def k1 = column[Int]("k1")
      def k2 = column[Int]("k2")
      def s = column[String]("s")
      def * = k1 ~ k2 ~ s
      def bFKTargetColumns(b: B.type) = (b.f1, b.f2)
      def bFK = foreignKey("b_fk", (k1, k2), B)(bFKTargetColumns _, ForeignKeyAction.NoAction, onDelete = ForeignKeyAction.Cascade)
    }

    object B extends Table[(Int, Int, String)]("b") {
      def f1 = column[Int]("f1")
      def f2 = column[Int]("f2")
      def s = column[String]("s")
      def * = f1 ~ f2 ~ s
      def bIdx1 = index("b_idx1", (f1, f2), unique = true)
    }

  }
  val s = showRaw(expr)
  println(s)
  println("===")
  println(expr)
}

object OldTests1 {
  import scala.reflect.runtime.universe._
  case class Supplier(id: Int, name: String, flag: Int)

  val expr = reify {

    // Definition of the SUPPLIERS table
    /*object Suppliers extends Tab[Supplier]("SUPPLIERS") {
      def id = column[Int]("SUP_ID")
      def name = column[String]("SUP_NAME")
      def flag = column[Int]("SUP_FLAG")
      def * = id ~ name ~ flag <> (Supplier, Supplier.unapply _)
//      def * = null
    }*/

    class A {
      val a = new A
      import a._
    }
  }
  val s = showRaw(expr)
  println(s)

  val n1 = This(TypeName("a"))
  val n2 = This(TypeName("b"))
  val s1 = Select(Select(n1, TermName("c")), TermName("d"))
  val s2 = Select(s1, TermName("e"))
  val t = List(s1, s2)
  //  val t3 = t.substituteThis(n1.symbol, n2)
  val t3 = new Transformer {
    override def transform(tree: Tree): Tree = {
      //      tree
      tree match {
        case i @ This(_) if i eq n1 => n2
        case _ => super.transform(tree)
      }
    }
  }.transformTrees(t)
  println(showRaw(t))
  println(showRaw(t3))
}