package com.typesafe.slick.testkit.tests

import java.util.logging.Logger

import com.typesafe.slick.testkit.util.{JdbcTestDB, TestkitTest}
import org.junit.Assert._

import scala.collection.mutable.ListBuffer
import scala.slick.lifted.ListShapeTrait

class ListShapeTest extends TestkitTest[JdbcTestDB] with ListShapeTrait {

  import tdb.profile.simple._

  class Score(tag: Tag, name: String) extends Table[(Int, Int, Int)](tag, name) {
    def id = column[Int]("id", O.PrimaryKey)

    def group = column[Int]("grp")

    def score = column[Int]("score")

    def * = (id, group, score)
  }

  def testBasic(): Unit = {
    def query(n: Column[Int], n2: Column[Int]) = {
      Query(List(List(List(1, 2).map(_.bind.asColumnOf[Int]) ++ List(n)))).
        map(x => x.map(x => x.map(x => x.map(x => x + 1).map(x => x + n2))))
    }
    val q = Compiled.apply(query _)
    val resultOne = q.apply(3, 1).run
    val expectedOne = List(List(List(1, 2) ++ List(3))).map(x => x.map(x => x.map(x => x + 1).map(x => x + 1)))
    assertEquals(List(expectedOne), resultOne)

    val resultTwo = Query(List(1, 2, 3)).map(t => t.apply(2) :: t.take(2).reverse).list
    val expectedTwo = List(3, 2, 1)
    assertEquals(List(expectedTwo), resultTwo)
  }

  def test {
    val score = TableQuery(new Score(_, "score_list"))
    (score.ddl).create
    def idSeed(i: Int): Stream[Int] = Stream.cons(i, idSeed(i + 1))
    val seed = idSeed(1).iterator
    val data = ListBuffer[(Int, Int, Int)]()
    score.++=(
      1.to(10).flatMap(x => 1.to(x).map(_ => ((seed.next(), 0, x)))) ++
        3.to(8).flatMap(x => 1.to(10 - x).map(_ => ((seed.next(), 1, x))))
    )

    /**
     * create table score (
     * id int primary key,
     * group int,
     * score int
     * )
     *
     * select b.group,
     * b.c1, b.c2, b.c3, ... b.c10,
     * (b.c1 + b.c2 + b.c3, ... b.c10)
     * from (
     * select a.group,
     * (select count(id) from score where group = a.group and score = 1) c1,
     * (select count(id) from score where group = a.group and score = 2) c2,
     * (select count(id) from score where group = a.group and score = 3) c3,
     * ...
     * (select count(id) from score where group = a.group and score = 10) c10
     * from (
     * select distinct group from score
     * ) a
     * ) b
     */

    def col(group: Column[Int]): List[(Int, Column[Int])] = {
      1.to(10).map(i => (i, score.filter(x => x.group === group && x.score === i).map(_.id).countDistinct)).toList
    }
    // below the Distinct is not executed currently. I don't know why.
    //    val queryOne = score.groupBy(t => t.group).map(t => t._1).
    //      map(t => (t, col(t))).
    //      filter(t => LiteralColumn(true) === LiteralColumn(true)).
    //      map(x => (x._1, x._2, x._2.map(x => x._2).reduceLeft(_ + _))).
    //      sortBy(t => t._1)
    val queryOne = Query(0).union(Query(1)).
      map(t => (t, col(t))).
      filter(t => LiteralColumn(true) === LiteralColumn(true)). // to introduce sub-query.
      map(t => (t._1, t._2, t._2.map(x => x._2).reduceLeft(_ + _))).
      sortBy(t => t._1)
    val resultOne = queryOne.list
    val expectedOne = ((0, 1.to(10).map(x => (x, x))) ::
      (1, 1.to(2).map(x => (x, 0)) ++ 3.to(8).map(x => (x, (10 - x))) ++ 9.to(10).map(x => (x, 0))) :: Nil).map {
      case (a, b) => (a, b, b.map(_._2).reduceLeft(_ + _))
    }
    assertEquals(expectedOne, resultOne)
    (score.ddl).drop
  }

}
