package com.typesafe.slick.testkit.tests

import com.typesafe.slick.testkit.util.{AsyncTest, RelationalTestDB, StandardTestDBs}

import scala.concurrent.Future

class ActionTest extends AsyncTest[RelationalTestDB] {
  import tdb.profile.api._

  def testSimpleActionAsFuture = {
    class T(tag: Tag) extends Table[Int](tag, u"t") {
      def a = column[Int]("a")
      def * = a
    }
    val ts = TableQuery[T]

    for {
      _ <- db.run {
        ts.schema.create >>
        (ts ++= Seq(2, 3, 1, 5, 4))
      }
      q1 = ts.sortBy(_.a).map(_.a)
      f1 = db.run(q1.result)
      r1 <- f1 : Future[Seq[Int]]
      _ = r1 shouldBe List(1, 2, 3, 4, 5)
    } yield ()
  }

  def testSessionPinning = {
    class T(tag: Tag) extends Table[Int](tag, u"t") {
      def a = column[Int]("a")
      def * = a
    }
    val ts = TableQuery[T]

    val aSetup = ts.schema.create andThen (ts ++= Seq(2, 3, 1, 5, 4))

    val aNotPinned = for {
      p1 <- IsPinned
      s1 <- GetSession
      l <- ts.length.result
      p2 <- IsPinned
      s2 <- GetSession
      _ = p1 shouldBe false
      _ = p2 shouldBe false
      _ = s1 shouldNotBe s2
    } yield ()

    val aFused = for {
      ((s1, l), s2) <- GetSession zip ts.length.result zip GetSession
      _ = s1 shouldBe s2
    } yield ()

    val aPinned = for {
      _ <- (for {
        p1 <- IsPinned
        s1 <- GetSession
        l <- ts.length.result
        p2 <- IsPinned
        s2 <- GetSession
        _ = p1 shouldBe true
        _ = p2 shouldBe true
        _ = s1 shouldBe s2
      } yield ()).withPinnedSession
      p3 <- IsPinned
      _ = p3 shouldBe false
    } yield ()

    aSetup andThen aNotPinned andThen aFused andThen aPinned
  }

  def testStreaming = {
    class T(tag: Tag) extends Table[Int](tag, u"t") {
      def a = column[Int]("a")
      def * = a
    }
    val ts = TableQuery[T]
    val q1 = ts.sortBy(_.a).map(_.a)

    val p1 = db.stream {
      ts.schema.create >>
      (ts ++= Seq(2, 3, 1, 5, 4)) >>
      q1.result
    }

    for {
      r1 <- materialize(p1)
      _ = r1 shouldBe Vector(1, 2, 3, 4, 5)
      r2 <- db.run(q1.result.head)
      _ = r2 shouldBe 1
      r3 <- db.run(q1.result.headOption)
      _ = r3 shouldBe Some(1)
    } yield ()
  }

  def testDeepRecursion = if(tdb == StandardTestDBs.H2Disk) {
    val a1 = DBIO.sequence((1 to 5000).toSeq.map(i => LiteralColumn(i).result))
    val a2 = DBIO.sequence((1 to 20).toSeq.map(i => if(i%2 == 0) LiteralColumn(i).result else DBIO.from(Future.successful(i))))
    val a3 = DBIO.sequence((1 to 20).toSeq.map(i => if((i/4)%2 == 0) LiteralColumn(i).result else DBIO.from(Future.successful(i))))
    val a4 = DBIO.seq((1 to 50000).toSeq.map(i => DBIO.successful("a4")): _*)
    val a5 = (1 to 50000).toSeq.map(i => DBIO.successful("a5")).reduceLeft(_ andThen _)
    val a6 = DBIO.fold((1 to 50000).toSeq.map(i => LiteralColumn(i).result), 0)(_ + _)
    val a7 = (1 to 10000).map(_ => DBIO.successful("a7")).reduceLeft((a, b) => a flatMap (_ => b) andThen b)

    DBIO.seq(
      a1.map(_ shouldBe (1 to 5000).toSeq),
      a2.map(_ shouldBe (1 to 20).toSeq),
      a3.map(_ shouldBe (1 to 20).toSeq),
      a4.map(_ shouldBe (())),
      a5.map(_ shouldBe "a5"),
      a6.map(_ shouldBe (1 to 50000).sum),
      a7.map(_ shouldBe "a7")
    )
  } else DBIO.successful(())

  def testOptionSequence = {
    class T(tag: Tag) extends Table[Option[Int]](tag, u"t") {
      def a = column[Int]("a")
      def * = a.?
    }
    val ts = TableQuery[T]

    val aSetup = ts.schema.create

    val a1 = LiteralColumn(Option(1)).result
    val a2 = DBIO.sequenceOption(Option(LiteralColumn(1).result))
    val a3 = DBIO.sequenceOption(Option.empty[DBIO[Int]])

    for {
      _ <- aSetup
      b1 <- a1
      b2 <- a2
      b3 <- a3
    } yield {
      b1 shouldBe b2
      b2 shouldNotBe b3
    }
  }

  def testFlatten = {
    class T(tag: Tag) extends Table[Int](tag, u"t") {
      def a = column[Int]("a")
      def * = a
    }
    val ts = TableQuery[T]
    for {
      _ <- db.run {
        ts.schema.create >>
          (ts ++= Seq(2, 3, 1, 5, 4))
      }
      needFlatten = for (_ <- ts.result) yield ts.result
      result <- db.run(needFlatten.flatten)
      _ = result shouldBe Seq(2, 3, 1, 5, 4)
    } yield ()
  }

  def testZipWith = {
      class T(tag: Tag) extends Table[Int](tag, u"t") {
        def a = column[Int]("a")
        def * = a
      }
      val ts = TableQuery[T]

      for {
        _ <- db.run {
          ts.schema.create >>
            (ts ++= Seq(2, 3, 1, 5, 4))
        }
        q1 = ts.sortBy(_.a).map(_.a).take(1)
        result <- db.run(q1.result.head.zipWith(q1.result.head)({ case (a, b) => a + b }))
        _ = result shouldBe 2
      } yield ()
    }

  def testCollect = {
    class T(tag: Tag) extends Table[Int](tag, u"t") {
      def a = column[Int]("a")

      def * = a
    }
    val ts = TableQuery[T]
    for {
      _ <- db.run {
        ts.schema.create >>
          (ts ++= Seq(2, 3, 1, 5, 4))
      }
      q1 = ts.sortBy(_.a).map(_.a).take(1)
      result <- db.run(q1.result.headOption.collect {
        case Some(a) => a
      })
      _ = result shouldBe 1
      _ = result shouldFail { _ =>
        val future = db.run(q1.result.headOption.collect {
          case None => ()
        })
        import scala.concurrent.duration.Duration
        import scala.concurrent.Await
        Await.result(future, Duration.Inf)
      }
    } yield ()
  }

  def testTruncate = {
    class T(_tag: Tag) extends Table[Int](_tag , "truncate_test"){
      def a = column[Int]("a")
      def * = a
    }

    val ts = TableQuery[T]
    for{
      _ <- ts.schema.create
      initial <- ts.result
      _ = assert(initial.toSet == Set())
      res <- (ts ++= Seq(2, 3, 1, 5, 4)) >>
             ts.result
      _ = assert(res.toSet == Set(2, 3, 1, 5, 4))
      newRes <- ts.schema.truncate >>
                ts.result
      _ = assert(newRes.toSet == Set())
    } yield ()
  }

  def testCreateIfNotExistsDropIfExists = {
    import scala.util.{Success, Failure}
    class T(_tag: Tag) extends Table[Int](_tag , "ddl_test"){
      def a = column[Int]("a")
      def * = a
    }

    class S(_tag: Tag) extends Table[String](_tag, "ddl_test2"){
      def a = column[String]("a2")
      def * = a
    }

    class A(tag: Tag) extends Table[(Int, Int, String)](tag, "a") {
      def k1 = column[Int]("k1")
      def k2 = column[Int]("k2")
      def s = column[String]("s")
      def * = (k1, k2, s)
      def pk = primaryKey("pk_a", (k1, k2))
    }

    val as = TableQuery[A]
    val ts = TableQuery[T]
    val ts2 = TableQuery[S]
    val batch = (ts.schema ++ ts2.schema)

    for{
      _ <- ts.schema.create
      _  <- ts2.schema.create
      _ <- ts.schema.createIfNotExists
      _ <- ts2.schema.createIfNotExists
      initial1 <- ts.result
      _ = assert(initial1.toSet == Set())
      initial2 <- ts2.result
      _ = assert(initial2.toSet == Set())
      res <- (ts ++= Seq(2, 3, 1, 5, 4)) >> ts.result
      _ = assert(res.toSet == Set(2, 3, 1, 5, 4))
      res2 <- (ts2 ++= Seq("2", "3", "1", "5", "4")) >> ts2.result
      _ = assert(res2.toSet == Set("2", "3", "1", "5", "4"))
      _ <- ts.schema.drop
      _ <- ts2.schema.drop
      _ <- ts.schema.dropIfExists
      _ <- ts2.schema.dropIfExists
      _ <- ts.schema.createIfNotExists
      _ <- ts.schema.create.asTry.map{
        case Failure(e:java.sql.SQLException) => ()
        case Failure(e: slick.SlickException) if tdb.profile == slick.memory.MemoryProfile => ()
        case Failure(e) => throw e
        case Success(_) => throw new Exception("Should have failed to create new table. Table exists")
      }
      _ <- ts2.schema.createIfNotExists
      _ <- ts2.schema.create.asTry.map{
        case Failure(e:java.sql.SQLException) => ()
        case Failure(e: slick.SlickException) if tdb.profile == slick.memory.MemoryProfile => ()
        case Failure(e) => throw e
        case Success(_) => throw new Exception("Should have failed to create new table. Table exists")
      }
      initial3 <- ts.result
      _ = assert(initial3.toSet == Set())
      initial4 <- ts2.result
      _ = assert(initial4.toSet == Set())
      _ <- ts.schema.dropIfExists
      _ <- ts.schema.drop.asTry.map{
        case Failure(e:java.sql.SQLException) => ()
        case Failure(e: slick.SlickException) if tdb.profile == slick.memory.MemoryProfile => ()
        case Failure(e) => throw e
        case Success(_) => throw new Exception("Should have failed to drop table. Table already dropped")
      }
      _ <- ts2.schema.dropIfExists
      _ <- ts2.schema.drop.asTry.map{
        case Failure(e:java.sql.SQLException) => ()
        case Failure(e: slick.SlickException) if tdb.profile == slick.memory.MemoryProfile => ()
        case Failure(e) => throw e
        case Success(_) => throw new Exception("Should have failed to drop table. Table already dropped")
      }
      //test batch create/drop
      _ <- batch.create
      _ <- batch.createIfNotExists
      res <- (ts ++= Seq(2, 3, 1, 5, 4)) >> ts.result
      _ = assert(res.toSet == Set(2, 3, 1, 5, 4))
      res2 <- (ts2 ++= Seq("2", "3", "1", "5", "4")) >> ts2.result
      _ = assert(res2.toSet == Set("2", "3", "1", "5", "4"))
      _ <- batch.drop
      _ <- batch.dropIfExists
      _ <- batch.createIfNotExists
      _ <- batch.create.asTry.map{
        case Failure(e:java.sql.SQLException) => ()
        case Failure(e: slick.SlickException) if tdb.profile == slick.memory.MemoryProfile => ()
        case Failure(e) => throw e
        case Success(_) => throw new Exception("Should have failed to create new table. Table exists")
      }
      _ <- batch.dropIfExists
      _ <- batch.drop.asTry.map{
        case Failure(e:java.sql.SQLException) => ()
        case Failure(e: slick.SlickException) if tdb.profile == slick.memory.MemoryProfile => ()
        case Failure(e) => throw e
        case Success(_) => throw new Exception("Should have failed to drop table. Table already dropped")
      }
      //test create/drop with constraints
      _ <- as.schema.createIfNotExists
      _ <- as ++= Seq((1, 1, "a11"), (1, 2, "a12"), (2, 1, "a21"), (2, 2, "a22") )
      _ <- (as += (1, 1, "a11-conflict")).failed
      _ <- as.schema.drop
    } yield ()
  }
}
