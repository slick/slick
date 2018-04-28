package slick.benchmark

import slick.compiler._
import slick.jdbc.H2Profile
import slick.jdbc.H2Profile.api._

import scala.collection.mutable.HashMap

/** Test query compiler performance with all queries from NewQuerySemanticsTest */
object CompilerBenchmark {
  val RUNS = 50
  val COUNT_CREATE = 1000
  val COUNT_TONODE = 1000
  val COUNT_COMPILE = 10

  def main(args: Array[String]): Unit = {
    System.setProperty("slick.verifyTypes", "false")
    System.setProperty("slick.detectRebuild", "false")
    println("Number of queries: "+allQueries.length)

    val phases = H2Profile.queryCompiler.phases
    val phaseNanos = new HashMap[String, Array[Long]]
    val compiler = new QueryCompiler(phases) {
      override def runPhase(p: Phase, state: CompilerState): CompilerState = {
        val t0 = System.nanoTime()
        val res = super.runPhase(p, state)
        val t1 = System.nanoTime()
        phaseNanos(p.name)(0) += (t1-t0)
        res
      }
    }

    var compileMS: Double = 0.0

    for(i <- 1 to RUNS) {
      val (queries, t1) = time("Creating queries", COUNT_CREATE)(allQueries)
      val (asts, t2) = time("Creating ASTs", COUNT_TONODE)(queries.map(_.toNode))
      phases.foreach(p => phaseNanos += (p.name -> new Array[Long](1)))
      //asts.zipWithIndex.foreach { case (n, i) => println(i); compiler.run(n) }
      /*if(i == RUNS-1) {
        println("Attach profiler and press Return")
        Console.readLine()
      }*/
      val (compiled, t3) = time("Compiling", COUNT_COMPILE)(asts.map(compiler.run(_)))
      /*if(i == RUNS-1) {
        println("Detach profiler and press Return")
        Console.readLine()
      }*/
      println(String.format("Creating: %1$7.3f ms, toNode: %2$7.3f ms, compiling: %3$7.3f ms", t1.asInstanceOf[AnyRef], t2.asInstanceOf[AnyRef], t3.asInstanceOf[AnyRef]))
      compileMS = t3
    }

    println("Last run by phase:")
    phases.foreach { p =>
      val pms = (phaseNanos(p.name)(0)/1000000.0/COUNT_COMPILE)
      val percentage = pms / compileMS * 100.0
      println(String.format("Phase %1$25s: %2$7.3f ms, %3$7.3f %%", p.name, pms.asInstanceOf[AnyRef], percentage.asInstanceOf[AnyRef]))
    }
  }

  def time[T](name: String, count: Int)(f: => T): (T, Double) = {
    val t0 = System.nanoTime()
    var res: T = null.asInstanceOf[T]
    var i = 0
    while(i < count) {
      res = f
      i += 1
    }
    val t1 = System.nanoTime()
    (res, (t1-t0)/1000000.0/count)
  }

  def allQueries = queriesFromNewComposition ++ queriesFromAdancedFusion ++ queriesFromExpansion ++ queriesFromNewFusion

  def queriesFromNewComposition: Vector[Rep[_]] = {
    class SuppliersStd(tag: Tag) extends Table[(Int, String, String, String, String, String)](tag, "SUPPLIERS") {
      def id = column[Int]("SUP_ID", O.PrimaryKey) // This is the primary key column
      def name = column[String]("SUP_NAME")
      def street = column[String]("STREET")
      def city = column[String]("CITY")
      def state = column[String]("STATE")
      def zip = column[String]("ZIP")
      def * = (id, name, street, city, state, zip)
    }
    val suppliersStd = TableQuery[SuppliersStd]

    class CoffeesStd(tag: Tag) extends Table[(String, Int, Int, Int, Int)](tag, "COFFEES") {
      def name = column[String]("COF_NAME", O.PrimaryKey, O.Length(254))
      def supID = column[Int]("SUP_ID")
      def price = column[Int]("PRICE")
      def sales = column[Int]("SALES")
      def total = column[Int]("TOTAL")
      def * = (name, supID, price, sales, total)
      def supplier = foreignKey("SUP_FK", supID, suppliersStd)(_.id)
    }
    val coffeesStd = TableQuery[CoffeesStd]

    class Suppliers(tag: Tag) extends Table[(Int, String, String)](tag, "SUPPLIERS") {
      def id = column[Int]("SUP_ID", O.PrimaryKey) // This is the primary key column
      def name = column[String]("SUP_NAME")
      def street = column[String]("STREET")
      def city = column[String]("CITY")
      def state = column[String]("STATE")
      def zip = column[String]("ZIP")
      def * = (id, name, street)
    }
    val suppliers = TableQuery[Suppliers]

    class Coffees(tag: Tag) extends Table[(String, Int, Int, Int, Int)](tag, "COFFEES") {
      def name = column[String]("COF_NAME", O.PrimaryKey)
      def supID = column[Int]("SUP_ID")
      def price = column[Int]("PRICE")
      def sales = column[Int]("SALES")
      def total = column[Int]("TOTAL")
      def * = (name, supID, price, sales, (total * 10))
      def totalComputed = sales * price
      def supplier = foreignKey("SUP_FK", supID, suppliers)(_.id)
    }
    val coffees = TableQuery[Coffees]

    val qa = for {
      c <- coffees.take(3)
    } yield (c.supID, (c.name, 42))
    val qa2 = coffees.take(3).map(_.name).take(2)
    val qb = qa.take(2).map(_._2)
    val qb2 = qa.map(n => n).take(2).map(_._2)
    val qc = qa.map(_._2).take(2)
    val q0 = coffees
    val q1 = for {
      c <- coffees.sortBy(c => (c.name, c.price.desc)).take(2)
      s <- suppliers
    } yield ((c.name, (s.city ++ ":")), c, s, c.totalComputed)
    val q1b_0 = coffees.sortBy(_.price).take(3) join suppliers on (_.supID === _.id)
    def q1b = for {
      (c, s) <- q1b_0.sortBy(_._1.price).take(2).filter(_._1.name =!= "Colombian")
      (c2, s2) <- q1b_0
    } yield (c.name, s.city, c2.name)
    val q2 = for {
      c <- coffees.filter(_.price < 900).map(_.*)
      s <- suppliers if s.id === c._2
    } yield (c._1, s.name)
    val q3 = coffees.flatMap { c =>
      val cf = Query(c).filter(_.price === 849)
      cf.flatMap { cf =>
        suppliers.filter(_.id === c.supID).map { s =>
          (c.name, s.name, cf.name, cf.total, cf.totalComputed)
        }
      }
    }
    val q3b = coffees.flatMap { c =>
      val cf = Query((c, 42)).filter(_._1.price < 900)
      cf.flatMap { case (cf, num) =>
        suppliers.filter(_.id === c.supID).map { s =>
          (c.name, s.name, cf.name, cf.total, cf.totalComputed, num)
        }
      }
    }
    def q4 = for {
      c <- coffees.map(c => (c.name, c.price, 42)).sortBy(_._1).take(2).filter(_._2 < 800)
    } yield (c._1, c._3)
    def q4b_0 = coffees.map(c => (c.name, c.price, 42)).filter(_._2 < 800)
    def q4b = for {
      c <- q4b_0
      d <- q4b_0
    } yield (c,d)
    val q5_0 = coffees.sortBy(_.price).take(2)
    val q5 = for {
      c1 <- q5_0
      c2 <- q5_0
    } yield (c1, c2)
    val q5b = for {
      t <- q5_0 join q5_0 on (_.name === _.name)
    } yield (t._1, t._2)
    val q6 = coffees.flatMap(c => suppliers)
    val q7a = for {
      c <- coffees.filter(_.price < 800) union coffees.filter(_.price > 950)
    } yield (c.name, c.supID, c.total)
    val q7 = for {
      c <- coffees.filter(_.price < 800).map((_, 1)) union coffees.filter(_.price > 950).map((_, 2))
    } yield (c._1.name, c._1.supID, c._2)
    val q71 = for {
      c <- coffees.filter(_.price < 800).map((_, 1))
    } yield (c._1.name, c._1.supID, c._2)
    val q7b = q7 filter (_._1 =!= "Colombian")
    val q8 = for {
      (c1, c2) <- coffees.filter(_.price < 900) joinLeft coffees.filter(_.price < 800) on (_.name === _.name)
    } yield (c1.name, c2.map(_.name))
    val q8b = for {
      t <- coffees.sortBy(_.sales).take(1) joinLeft coffees.sortBy(_.sales).take(2) on (_.name === _.name) joinLeft coffees.sortBy(_.sales).take(4) on (_._1.supID === _.supID)
    } yield (t._1, t._2)

    Vector(qa, qa2, qb, qb2, qc, q0, q1, q1b_0, q1b, q2, q3, q3b, q4, q4b_0, q4b, q5_0, q5, q5b, q6, q7a, q1, q71, q7b, q8, q8b)
  }

  def queriesFromAdancedFusion: Vector[Rep[_]] = {
    class TableA(tag: Tag) extends Table[Int](tag, "TableA") {
      def id = column[Int]("id")
      def * = id
    }
    val tableA = TableQuery[TableA]

    class TableB(tag: Tag) extends Table[(Int, Int)](tag, "TableB") {
      def id = column[Int]("id")
      def start = column[Int]("start")
      def * = (id, start)
    }
    val tableB = TableQuery[TableB]

    class TableC(tag: Tag) extends Table[Int](tag, "TableC") {
      def start = column[Int]("start")
      def * = start
    }
    val tableC = TableQuery[TableC]

    val queryErr2 = for {
      a <- tableA
      b <- tableB if b.id === a.id
      start = a.id + 1
      c <- tableC if c.start <= start
    } yield (b, c)

    Vector(queryErr2)
  }

  def queriesFromExpansion: Vector[Rep[_]] = {
    class A(tag: Tag) extends Table[(Int, String)](tag, "A_refexp") {
      def id = column[Int]("id")
      def a = column[String]("a")
      def b = column[String]("b")
      def * = (id, a)
      override def create_* = collectFieldSymbols((id, a, b).shaped.toNode)
    }
    val as = TableQuery[A]

    val q1 = as.map(identity).filter(_.b === "b3")
    val q2a = as.sortBy(_.a) join as on (_.b === _.b)
    val q2 = for {
      (c, s) <- q2a
      c2 <- as
    } yield (c.id, c2.a)

    Vector(q1, q2a, q2)
  }

  def queriesFromNewFusion: Vector[Rep[_]] = {
    class A(tag: Tag) extends Table[(Int, String, String)](tag, "A_NEWFUSION") {
      def id = column[Int]("id")
      def a = column[String]("a")
      def b = column[String]("b")
      def * = (id, a, b)
    }
    val as = TableQuery[A]

    val q1 = (as join as on (_.id === _.id))
    val q2 = (as join as on (_.id === _.id) join as on (_._1.id === _.id))
    val q3 = q2.map { case ((a1, a2), a3) => (a1.id, a2.a, a3.b) }
    val q4 = as.map(a => (a.id, a.a, a.b, a)).filter(_._3 === "b").map { case (id, a1, b, a2) => (id, a2) }
    val q5a = as.to[Set].filter(_.b === "b").map(_.id)
    val q5b = as.filter(_.b === "b").to[Set].map(_.id)
    val q5c = as.filter(_.b === "b").map(_.id).to[Set]
    val q6 = (as join as).groupBy(j => (j._1.a, j._1.b)).map { case (ab, rs) => (ab, rs.length, rs.map(_._1).length, rs.map(_._2).length, rs.map(_._1.id).max, rs.map(_._1.id).length) }
    val q7 = q6.filter(_._1._1 === "a").map(_._5.getOrElse(0))
    val q8 = as.sortBy(_.id.desc).map(_.a)
    val q9a = as.sortBy(_.b).sortBy(_.a.desc).map(_.id)
    val q9b = as.sortBy(a => (a.a.desc, a.b)).map(_.id)
    val q10 = (as join as).map { case (a1, a2) => a1.id * 3 + a2.id - 3 }.sorted
    val q11a = q10.take(5)
    val q11b = q10.take(5).take(3)
    val q11c = q10.take(5).take(3).drop(1)
    val q11d = q10.take(5).drop(1).take(3)
    val q11e = q10.drop(7)
    val q11f = q10.take(6).drop(2).filter(_ =!= 5)
    val q12 = as.filter(_.id <= as.map(_.id).max-1).map(_.a)
    val q13 = (as.filter(_.id < 2) union as.filter(_.id > 2)).map(_.id)
    val q14 = q13.to[Set]
    val q15 = (as.map(a => a.id.?).filter(_ < 2) unionAll as.map(a => a.id.?).filter(_ > 2)).map(_.get).to[Set]
    val q16 = (as.map(a => a.id.?).filter(_ < 2) unionAll as.map(a => a.id.?).filter(_ > 2)).map(_.getOrElse(-1)).to[Set].filter(_ =!= 42)
    val q17 = as.sortBy(_.id).zipWithIndex.filter(_._2 < 2L).map { case (a, i) => (a.id, i) }

    Vector[Rep[_]](q1, q2, q3, q4, q5a, q5b, q5c, q6, q7, q8, q9a, q9b, q10, q11a, q11b, q11c, q11d, q11e, q11f, q12, q13, q14, /*q15,*/ q16, q17)
  }
}
