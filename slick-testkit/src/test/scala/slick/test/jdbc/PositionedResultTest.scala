package slick.test.jdbc

import slick.jdbc.{PositionedResult, PositionedResultIterator}
import com.typesafe.slick.testkit.util.DelegateResultSet
import org.junit.Test
import org.junit.Assert._

class PositionedResultTest {

  @Test def testMaxRows: Unit = {
    assertEquals(5, createFakePR(5, 0).length)
    assertEquals(1, createFakePR(5, 1).length)
    assertEquals(4, createFakePR(5, 4).length)
    assertEquals(5, createFakePR(5, 5).length)
    assertEquals(5, createFakePR(5, 6).length)
  }

  def createFakePR(len: Int, limit: Int): PositionedResultIterator[Int] = {
    val fakeRS = new DelegateResultSet(null) {
      var count: Int = 0
      override def next() = {
        count += 1
        count <= len
      }
      override def getInt(columnIndex: Int): Int = columnIndex
      override def wasNull(): Boolean = false
    }
    val pr = new PositionedResult(fakeRS) { def close(): Unit = {} }
    new PositionedResultIterator[Int](pr, limit, true) {
      def extractValue(pr: PositionedResult) = pr.nextInt()
    }
  }

}
