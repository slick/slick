package slick.test.jdbc

import java.util.UUID

import org.junit.Test
import org.junit.Assert._

import cats.effect.unsafe.implicits.global

import slick.relational.RelationalProfile

import com.typesafe.slick.testkit.util.{DBTest, DBTestObject, JdbcTestDB}
import com.typesafe.slick.testkit.util.StandardTestDBs._

object HsqldbUUIDTest extends DBTestObject(HsqldbMem)

/** HSQLDB stores `java.util.UUID` in its native `UUID` column type and still supports `BINARY(16)` columns. */
class HsqldbUUIDTest(val tdb: JdbcTestDB) extends DBTest {
  import tdb.profile.api._

  // In unsigned byte order, which is how HSQLDB sorts UUID and BINARY values
  val u1 = UUID.fromString("00000000-0000-0000-0000-000000000001")
  val u2 = UUID.fromString("7fffffff-0000-0000-0000-000000000002")
  val u3 = UUID.fromString("ffffffff-0000-0000-0000-000000000003")
  val u4 = UUID.fromString("80000000-0000-0000-0000-000000000004")

  val uuidType = tdb.profile.columnTypes.uuidJdbcType

  class Parents(tag: Tag, tableName: String) extends Table[(UUID, Int)](tag, tableName) {
    def id = column[UUID]("id", O.PrimaryKey)
    def n = column[Int]("n")
    def * = (id, n)
  }

  class Children(tag: Tag, parents: TableQuery[Parents]) extends Table[(Int, Option[UUID])](tag, "children") {
    def id = column[Int]("id", O.PrimaryKey)
    def parentId = column[Option[UUID]]("parent_id")
    def * = (id, parentId)
    def parent = foreignKey("parent_fk", parentId, parents)(_.id.?)
  }

  /** Insert, read, filter, sort and update UUID values in the table, with plain JDBC byte access mixed in */
  def checkTable(parents: TableQuery[Parents]): DBIO[Unit] = {
    val tableName = parents.baseTableRow.tableName
    val insertBytes = SimpleDBIO { ctx =>
      val st = ctx.connection.prepareStatement(s"""insert into "$tableName" values (?, ?)""")
      try {
        st.setBytes(1, uuidType.toBytes(u3))
        st.setInt(2, 3)
        st.executeUpdate()
      } finally st.close()
    }
    val readBytes = SimpleDBIO { ctx =>
      val st = ctx.connection.createStatement()
      try {
        val rs = st.executeQuery(s"""select "id" from "$tableName" order by "n"""")
        val b = Vector.newBuilder[UUID]
        while (rs.next()) b += uuidType.fromBytes(rs.getBytes(1))
        b.result()
      } finally st.close()
    }
    DBIO.seq(
      parents ++= Seq((u2, 2), (u1, 1)),
      insertBytes,
      readBytes.map(r => assertEquals(Vector(u1, u2, u3), r)),
      parents.sortBy(_.id).map(_.n).result.map(r => assertEquals(Seq(1, 2, 3), r)),
      parents.filter(_.id === u3.bind).map(_.n).result.map(r => assertEquals(Seq(3), r)),
      parents.filter(_.id === LiteralColumn(u2)).map(_.n).result.map(r => assertEquals(Seq(2), r)),
      parents.filter(_.id.inSet(Seq(u1, u3))).map(_.n).result.map(r => assertEquals(Set(1, 3), r.toSet)),
      sql"""select "n" from "#$tableName" where "id" = #${uuidType.valueToSQLLiteral(u1)}""".as[Int]
        .map(r => assertEquals(Seq(1), r)),
      parents.filter(_.n === 3).map(_.id).update(u4),
      parents.filter(_.n === 3).map(_.id).result.map(r => assertEquals(Seq(u4), r)),
      parents.sortBy(_.id).map(_.n).result.map(r => assertEquals(Seq(1, 2, 3), r))
    )
  }

  // The in-memory database only lives as long as a session is open
  def run(action: DBIO[Any]): Unit = runBlocking(action.withPinnedSession)

  @Test def testNativeUUID(): Unit = {
    val parents = TableQuery(new Parents(_, "parents"))
    val children = TableQuery(new Children(_, parents))

    assertTrue(parents.schema.createStatements.mkString.contains(""""id" UUID NOT NULL PRIMARY KEY"""))
    run(DBIO.seq(
      (parents.schema ++ children.schema).create,
      checkTable(parents),
      children ++= Seq((1, Some(u1)), (2, None)),
      children.join(parents).on(_.parentId === _.id).map { case (c, p) => (c.id, p.id) }.result
        .map(r => assertEquals(Seq((1, u1)), r)),
      (children += ((3, Some(UUID.randomUUID())))).asTry
        .map(r => assertTrue("foreign key violation expected", r.isFailure))
    ))
  }

  @Test def testLegacyBinaryColumn(): Unit = {
    // A table as created by Slick versions that mapped UUID to BINARY(16) on HSQLDB
    val parents = TableQuery(new Parents(_, "legacy_parents"))
    run(DBIO.seq(
      sqlu"""create table "legacy_parents" ("id" BINARY(16) NOT NULL PRIMARY KEY, "n" INTEGER NOT NULL)""",
      checkTable(parents)
    ))
  }

  @Test def testModel(): Unit = {
    val model = db.run((
      sqlu"""create table "model" (
               "uuid" UUID NOT NULL,
               "uuid_def" UUID DEFAULT '2f3f866c-d8e6-11e2-bb56-50e549c9b654',
               "uuid_null" UUID DEFAULT NULL,
               "bin" BINARY(16))""" >>
        tdb.profile.createModel(ignoreInvalidDefaults = false)
    ).withPinnedSession).unsafeRunSync()
    val columns = model.tables.flatMap(_.columns).map(c => c.name -> c).toMap
    assertEquals("java.util.UUID", columns("uuid").tpe)
    assertEquals("java.util.UUID", columns("uuid_def").tpe)
    assertEquals("java.sql.Blob", columns("bin").tpe)
    assertEquals(
      Set(Some(UUID.fromString("2f3f866c-d8e6-11e2-bb56-50e549c9b654"))),
      columns("uuid_def").options.collect { case RelationalProfile.ColumnOption.Default(v) => v }
    )
    assertEquals(
      Set(None),
      columns("uuid_null").options.collect { case RelationalProfile.ColumnOption.Default(v) => v }
    )
  }
}
