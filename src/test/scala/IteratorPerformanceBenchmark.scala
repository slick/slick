import collection.mutable.ArrayBuffer
import org.scalaquery._
import org.scalaquery.session._
import org.scalaquery.session.Database.threadLocalSession
import org.scalaquery.util._
import org.scalaquery.ql._
import org.scalaquery.ql.extended.{ExtendedTable => Table}
import org.scalaquery.ql.extended.H2Driver.Implicit._

object IteratorPerformanceBenchmark {
  def main(args: Array[String]) {
    val Props = new Table[(String, String)]("properties") {
      def key = column[String]("key", O.PrimaryKey)
      def value = column[String]("value")
      def * = key ~ value
    }
    Database.forURL("jdbc:h2:mem:test1", driver = "org.h2.Driver") withSession {
      Props.ddl.create
      val count = 10000
      val size = 1000
      for(i <- 1 to size) Props.insert("k"+i, "v"+i)
      val inv = Query(Props).invoker

      val buf = new ArrayBuffer[(String, String)]
      def measure(s: String)(f: => Any) {
        val t0 = System.currentTimeMillis()
        var i = 0
        while(i < count) { buf.clear; f; i += 1; assert(buf.length == size) }
        val t1 = System.currentTimeMillis()
        println(s+" ("+count+"x) took "+(t1-t0)+"ms, size = "+buf.length)
      }

      val repeat = 5
      var r = 0
      while(r < repeat) {
        measure("foreach"){ ; inv.foreach((), { i => buf += i }, 0) }
        measure("elements loop"){
          val it = inv.elements()
          try {
            while(it.hasNext) buf += it.next
          } finally { it.close() }
        }
        measure("elements.foreach"){ inv.elements().foreach(i => buf += i) }
        r += 1
      }
    }
  }


  /*def main2(args: Array[String]) {
    val inv = new Invoker[Int, String] {
      def foreach(param: Int, f: String => Unit, maxRows: Int)(implicit session: Session): Unit = {
        var i = 0
        while(i < param) {
          f(i.toString)
          i += 1
        }
      }

      def elements(param: Int)(implicit session: Session): CloseableIterator[String] = {
        new CloseableIterator[String] {
          private[this] var i = 0
          def next() = if(!hasNext) noNext else { val r = i; i += 1; r.toString }
          def hasNext: Boolean = i < param
          override def close() {}
        }
      }
    }

    val buf = new ArrayBuffer[String]
    inv.foreach(5, { i => buf += i }, -1)(null)
    println("with foreach: "+buf)
    buf.clear
    inv.elements(5)(null).foreach(i => buf += i)
    println("with elements: "+buf)

    val count = 100000
    val size = 1000

    def measure(s: String)(f: => Any) {
      val t0 = System.currentTimeMillis()
      var i = 0
      while(i < count) {
        f
        i += 1
      }
      val t1 = System.currentTimeMillis()
      println(s+"("+count+" * "+size+") took "+(t1-t0)+"ms")
    }

    while(true) {
      measure("elements"){ buf.clear; inv.elements(size)(null).foreach(i => buf += i) }
      measure("foreach"){ buf.clear; inv.foreach(size, { i => buf += i }, -1)(null) }
    }
  }*/
}
