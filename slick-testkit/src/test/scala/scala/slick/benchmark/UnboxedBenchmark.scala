package scala.slick.benchmark

import scala.slick.ast._
import scala.slick.jdbc._
import scala.slick.relational._
import com.typesafe.slick.testkit.util.DelegateResultSet

object UnboxedBenchmark extends App {

  val driver = scala.slick.driver.H2Driver
  import driver.simple._

  case class A(var a: Int, var b: Int, var c: Int, var d: Int)

  class ARow(tag: Tag) extends Table[(Int, Option[Int], Option[Int])](tag, "A") {
    def i = column[Int]("A")
    def s = column[Option[Int]]("B")
    def n = column[Option[Int]]("C")
    def * = (i, s, n)
    def proj = (i, s.get, s.getOrElse(-1), n.getOrElse(-1))
  }
  val as = TableQuery[ARow]

  // Standard converters
  val q1 =  as.map(a => a.proj <> (A.tupled, A.unapply))

  // Fast path
  val q2 =  as.map(a => a.proj <> (A.tupled, A.unapply)
    fastPath(new FastPath(_) {
      val (a, b, c, d) = (nextInt, nextInt, nextInt, nextInt)
      override def readGeneric(r: Reader) = new A(a.read(r), b.read(r), c.read(r), d.read(r))
    })
  )

  // Allocation-free fast path
  val sharedA = new A(0, 0, 0, 0)
  val q3 =  as.map(a => a.proj <> (A.tupled, A.unapply)
    fastPath(new FastPath(_) {
      val (a, b, c, d) = (nextInt, nextInt, nextInt, nextInt)
      override def readGeneric(r: Reader) = {
        sharedA.a = a.read(r)
        sharedA.b = b.read(r)
        sharedA.c = c.read(r)
        sharedA.d = d.read(r)
        sharedA
      }
    })
  )

  runTest(q1.toNode)
  runTest(q2.toNode)
  runTest(q3.toNode)

  def runTest(n: Node) {
    val rsm = driver.queryCompiler.run(n).tree
    Dump(rsm)
    val ResultSetMapping(_, _, CompiledMapping(converter, _)) = rsm
    ResultConverter.dump(converter)
    for(i <- 1 to 5) {
      val pr = createFakePR(10000000, converter.asInstanceOf[ResultConverter[JdbcResultConverterDomain, _]])
      readPR(pr)
    }
  }

  def createFakePR(len: Long, converter: ResultConverter[JdbcResultConverterDomain, _]): PositionedResultIterator[Any] = {
    val fakeRS = new DelegateResultSet(null) {
      var count: Long = 0
      var lastIndex: Int = 0
      override def next() = {
        count += 1
        count <= len
      }
      override def getInt(columnIndex: Int): Int = {
        lastIndex = columnIndex
        columnIndex
      }
      override def wasNull(): Boolean = lastIndex >= 3
    }
    new PositionedResultIterator[Any](fakeRS, 0) {
      def extractValue = converter.readGeneric(this)
      def closeUnderlying() {}
    }
  }

  def readPR(pr: PositionedResultIterator[_]): (Long, AnyRef) = {
    val t0 = System.currentTimeMillis()
    var count: Long = 0
    var lastRow: AnyRef = null
    while(pr.hasNext) {
      lastRow = pr.next.asInstanceOf[AnyRef]
      count += 1
    }
    val time = System.currentTimeMillis() - t0
    println("Read "+count+" rows in "+time+" ms. Last row was: "+lastRow)
    (count, lastRow)
  }
}
