import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.concurrent.ExecutionContext.Implicits.global
import java.sql.Date

import slick.jdbc.H2Profile.api._

/** This example shows how to lift a native database function
  * to Slick's query language.
  */
object CallNativeDBFunction extends App {

  class SalesPerDay(tag: Tag) extends Table[(Date, Int)](tag, "SALES_PER_DAY") {
    def day = column[Date]("DAY", O.PrimaryKey)
    def count = column[Int]("COUNT")
    def * = (day, count)
  }
  val salesPerDay = TableQuery[SalesPerDay]

  val dayOfWeek = SimpleFunction.unary[Date, Int]("day_of_week")

  val dayOfWeek2 = SimpleFunction[Int]("day_of_week")

  def dayOfWeek3(c: Rep[Date]) = dayOfWeek2(Seq(c))

  // Use the lifted function in a query to group by day of week
  val q1 = for {
    (dow, q) <- salesPerDay
      .map(s => (dayOfWeek(s.day), s.count))
      .groupBy(_._1)
  } yield (dow, q.map(_._2).sum)

  val db = Database.forConfig("h2")
  val f = db.run(DBIO.seq(
    salesPerDay.schema.create,
    salesPerDay ++= Seq(
      (Date.valueOf("2011-04-01"), 3),
      (Date.valueOf("2011-04-02"), 8),
      (Date.valueOf("2011-04-03"), 0),
      (Date.valueOf("2011-04-04"), 2),
      (Date.valueOf("2011-04-05"), 1),
      (Date.valueOf("2011-04-06"), 1),
      (Date.valueOf("2011-04-07"), 2),
      (Date.valueOf("2011-04-08"), 5),
      (Date.valueOf("2011-04-09"), 4),
      (Date.valueOf("2011-04-10"), 0),
      (Date.valueOf("2011-04-11"), 2)
    ),
    q1.result.map { r =>
      println("Day of week (1 = Sunday) -> Sales:")
      r.foreach { case (dow, sum) => println("  " + dow + "\t->\t" + sum) }
    }
  ))

  Await.result(f, Duration.Inf)
  db.close()
}
