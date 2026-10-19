package slick.test.lifted

import org.junit.Assert._
import org.junit.Test
import slick.ast.Node
import slick.compiler.CompilerState
import slick.jdbc.{JdbcActionComponent, OracleProfile}

class OracleDMLTest {
  import slick.jdbc.OracleProfile.api._

  @Test def testOnlyPrimaryKeysComposite(): Unit = {
    class TableWithOnlyIdColumns(tag: Tag) extends Table[(Int, Int)](tag, "mytable") {
      def id = column[Int]("id")
      def otherId = column[Int]("otherId")

      def pk = primaryKey( "SampleCompositePk", ( id, otherId ) )
      def * = (id, otherId)
    }

    assertFalse("When there are only primary keys columns there shouldn't be an update",
      TableQuery[TableWithOnlyIdColumns].insertOrUpdate((1, 1)).statements.mkString.contains("when matched then update set"))
  }

  @Test def testCompositePrimaryKeyAndOtherColumns(): Unit = {
    class TableWithOnlyIdColumns(tag: Tag) extends Table[(Int, Int, Option[String])](tag, "mytable") {
      def id = column[Int]("id")
      def otherId = column[Int]("otherId")
      def optionalColumn = column[Option[String]]("optionalColumn")


      def pk = primaryKey( "SampleCompositePk", ( id, otherId ) )
      def * = (id, otherId, optionalColumn)
    }

    assertTrue("Should update columns besides those on the primary key ",
      TableQuery[TableWithOnlyIdColumns].insertOrUpdate((1, 1, Some(""))).statements.mkString.contains("when matched then update set t.\"optionalColumn\"=s.\"optionalColumn\""))
  }

  @Test def testOnlyPrimaryKeysSinglePrimaryKey(): Unit = {
    class TableWithOnlyIdColumns(tag: Tag) extends Table[Int](tag, "mytable") {
      def id = column[Int]("id", O.PrimaryKey)

      def * = id
    }

    assertFalse("When there are only primary keys columns there shouldn't be an update",
      TableQuery[TableWithOnlyIdColumns].insertOrUpdate(1).statements.mkString.contains("when matched then update set"))
  }

  @Test def testPrimaryKeyAndOtherColumn(): Unit = {
    class TableWithOnlyIdColumns(tag: Tag) extends Table[(Int, Option[String])](tag, "mytable") {
      def id = column[Int]("id", O.PrimaryKey)
      def optionalColumn = column[Option[String]]("optionalColumn")

      def * = (id,  optionalColumn)
    }

    assertTrue("Should update columns besides those on the primary key ",
      TableQuery[TableWithOnlyIdColumns].insertOrUpdate((1, Some(""))).statements.mkString.contains("when matched then update set t.\"optionalColumn\"=s.\"optionalColumn\""))
  }

  class BooleanTable(tag: Tag) extends Table[(Int, Boolean, Option[Boolean])](tag, "t") {
    def a = column[Int]("a")
    def b = column[Boolean]("b")
    def ob = column[Option[Boolean]]("ob")
    def * = (a, b, ob)
  }
  val booleanTable = TableQuery[BooleanTable]

  private def sql(q: Query[?, ?, Seq]) = q.result.statements.mkString.replaceAll("\\s+", " ")

  @Test def testUnionCastsBooleansToNumbers(): Unit = {
    val ts = booleanTable
    val columns = ts.map(t => (t.a, t.b))
    // Boolean columns are CHAR(1) and Boolean literals are numbers, so the column is cast to a number
    val literal = sql(columns.union(ts.map(t => (t.a, LiteralColumn(true)))))
    assertTrue(literal, literal.contains("""cast("b" as NUMBER(1))""") && literal.contains(" 1 as "))
    assertFalse(literal, literal.contains("cast(1 "))
    val parameter = sql(columns.union(ts.map(t => (t.a, false.bind))))
    assertTrue(parameter, parameter.contains("""cast("b" as NUMBER(1))"""))
    val expression = sql(ts.map(t => (t.a, t.a > 3)).union(ts.map(t => (t.a, t.b))))
    assertTrue(expression, expression.contains("""cast("b" as NUMBER(1))"""))
    val option = sql(ts.map(t => (t.a, t.ob)).union(ts.map(t => (t.a, LiteralColumn(Option(true))))))
    assertTrue(option, option.contains("""cast("ob" as NUMBER(1))"""))
    // A bound NULL is a number; an inline NULL takes the type of the other side and is not cast
    val boundNone = sql(ts.map(t => (t.a, t.ob)).union(ts.map(t => (t.a, LiteralColumn(Option.empty[Boolean]).bind))))
    assertTrue(boundNone, boundNone.contains("""cast("ob" as NUMBER(1))"""))
    // A column of a nested union is still a column; once cast, it is a number
    val nested = sql(columns.union(columns).union(ts.map(t => (t.a, LiteralColumn(true)))))
    assertTrue(nested, nested.contains(""" cast(x2.x5 as NUMBER(1)) """))
    assertFalse(nested, nested.contains("""cast("b" """))
    val castInNested = sql(ts.map(t => (t.a, LiteralColumn(true))).union(columns).union(columns))
    assertTrue(castInNested, castInNested.contains(""" 1 as x5 """) && castInNested.contains("""cast("b" as NUMBER(1)) as x6"""))
    assertFalse(castInNested, castInNested.contains("cast(x2."))
    val join = sql(ts.join(ts).on(_.a === _.a).map { case (x, y) => (x.b, y.b) }.union(ts.map(t => (t.b, LiteralColumn(true)))))
    assertTrue(join, join.contains("""cast(x4."b" as NUMBER(1))"""))
    assertFalse(join, join.contains("""cast(x2."b" """))
  }

  @Test def testUnionKeepsMatchingBooleans(): Unit = {
    val ts = booleanTable
    // Both sides are columns, or both are numbers: no cast
    val statements = Seq(
      sql(ts.map(t => (t.a, t.b)).union(ts.map(t => (t.a, t.b)))),
      sql(ts.join(ts).on(_.a === _.a).union(ts.join(ts).on(_.a === _.a))),
      sql(ts.map(t => (t.b, t.ob)).unionAll(ts.map(t => (t.b, t.b.?)))),
      sql(ts.map(_.b).union(ts.map(_.b)).union(ts.map(_.b))),
      sql(ts.map(t => (t.a, t.b)).union(ts.map(t => (t.a, t.b)).filter(_._1 > 1).sortBy(_._1).take(3))),
      sql(ts.map(t => (t.a, t.ob)).union(ts.map(t => (t.a, LiteralColumn(Option.empty[Boolean]))))),
      sql(ts.map(t => (t.a, LiteralColumn(true))).union(ts.map(t => (t.a, false.bind)))),
      sql(ts.map(t => (t.a, t.a < 3)).union(ts.map(t => (t.a, t.a > 3)))),
      sql(ts.filter(_.b.in(ts.filter(_.a > 1).map(_.b).union(ts.map(_.b)))).map(_.a))
    )

    for (s <- statements) assertFalse(s, s.contains("cast("))
  }

  /* Each union is classified from the classification of its sides. Classifying the sides again at
   * every level took a number of calls exponential in the length of the chain. The chain is short
   * because longer ones overflow the stack in the query compiler (#3885). */
  @Test def testLongUnionChain(): Unit = {
    var calls = 0
    object CountingProfile extends OracleProfile with JdbcActionComponent.OneRowPerStatementOnly {
      override def createQueryBuilder(n: Node, state: CompilerState): QueryBuilder = new OracleQueryBuilder(n, state) {
        override private[slick] def resultBooleanSql(n: Node) = {
          calls += 1
          super.resultBooleanSql(n)
        }
      }
    }
    val q = booleanTable.map(t => (t.a, t.b))
    val chain = (1 to 12).foldLeft(q)((u, _) => u.union(q))
    def countingSql(q: Query[?, ?, Seq]) =
      CountingProfile.api.streamableQueryActionExtensionMethods(q).result.statements.mkString.replaceAll("\\s+", " ")
    val columns = countingSql(chain)
    val withLiteral = countingSql(chain.union(booleanTable.map(t => (t.a, LiteralColumn(true)))))

    assertFalse(columns, columns.contains("cast("))
    // Only the outermost union casts the column
    assertEquals(withLiteral, 1, "cast\\(".r.findAllIn(withLiteral).length)
    // 120 calls; classifying the sides of each union again took 65474
    assertTrue(s"$calls calls", calls < 200)
  }
}
