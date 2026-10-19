package com.typesafe.slick.testkit.tests

import com.typesafe.slick.testkit.util.{AsyncTest, RelationalTestDB}

/** Long chains of `||` and `&&` built with `reduce` must compile, render and run without
  * overflowing the default thread stack (https://github.com/slick/slick/issues/1606). */
class LongBooleanChainTest extends AsyncTest[RelationalTestDB] {
  import tdb.profile.api._

  class T(tag: Tag) extends Table[(Int, Int, Option[Int])](tag, "long_boolean_chain") {
    def id = column[Int]("id", O.PrimaryKey)
    def a = column[Int]("a")
    def o = column[Option[Int]]("o")
    def * = (id, a, o)
  }
  lazy val ts = TableQuery[T]

  // ids 1 to 10, a = id % 3, o = Some(id) for odd ids
  private val rows = (1 to 10).map(i => (i, i % 3, if (i % 2 == 1) Some(i) else None))

  /* Derby's own SQL compiler recurses over OR lists (OrNode.preprocess) and overflows the stack
   * at a few thousand conditions, independently of how Slick renders them, and it rejects the
   * mixed query below as "too complex" at 500 + 500 conditions. */
  private def n = if (tdb.profile == slick.jdbc.DerbyProfile) 1000 else 2000

  def testLongChains = {
    val orChain = ts.filter(t => (1 to n).map(x => t.id === (x * 3).bind).reduce(_ || _))
    val andChain = ts.filter(t => (1 to n).map(x => t.id =!= x * 3).reduce(_ && _))
    val optionOrChain = ts.filter(t => (1 to n).map(x => t.o === x * 3).reduce(_ || _))
    val mixed = ts.filter { t =>
      (1 to n / 4).map(x => t.id === x && t.a =!= 1).reduce(_ || _) &&
        (1 to n / 4).map(x => t.id === x * 2).reduce(_ || _)
    }
    seq(
      ts.schema.create,
      ts ++= rows,
      orChain.map(_.id).sorted.result.map(_ shouldBe Seq(3, 6, 9)),
      andChain.map(_.id).sorted.result.map(_ shouldBe Seq(1, 2, 4, 5, 7, 8, 10)),
      optionOrChain.map(_.id).sorted.result.map(_ shouldBe Seq(3, 9)),
      mixed.map(_.id).sorted.result.map(_ shouldBe Seq(2, 6, 8)),
      orChain.length.result.map(_ shouldBe 3)
    )
  }
}
