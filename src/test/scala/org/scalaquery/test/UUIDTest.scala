package org.scalaquery.test

import javax.sql.rowset.serial.SerialBlob
import org.junit.Test
import org.junit.Assert._
import org.scalaquery.ql._
import org.scalaquery.ql.basic.{BasicTable => Table, BasicProfile}
import org.scalaquery.session._
import org.scalaquery.session.Database.threadLocalSession
import org.scalaquery.test.util._
import org.scalaquery.test.util.TestDB._
import java.util.UUID

object UUIDTest extends DBTestObject(Postgres)

class UUIDTest(tdb: TestDB) extends DBTest(tdb) {
  import tdb.driver.Implicit._

  implicit object UUIDTypeMapper extends BaseTypeMapper[UUID] with TypeMapperDelegate[UUID] {
    def apply(p: BasicProfile) = this
    val zero = new UUID(0, 0)
    val sqlType = java.sql.Types.OTHER
    override def sqlTypeName = "uuid"
    def setValue(v: UUID, p: PositionedParameters) = p.setOther(v)
    def setOption(v: Option[UUID], p: PositionedParameters) = p.setOtherOption(v)
    def nextValue(r: PositionedResult) = r.nextOther().asInstanceOf[UUID]
    def updateValue(v: UUID, r: PositionedResult) = r.updateOther(v)
    override def valueToSQLLiteral(value: UUID) = "'" + value + "'"
  }

  object T1 extends Table[(Int, Option[UUID])]("test") {
    def id = column[Int]("id")
    def data = column[Option[UUID]]("data")
    def * = id ~ data
  }

  object T2 extends Table[(Int, UUID)]("test2") {
    def id = column[Int]("id", O PrimaryKey)
    def data = column[UUID]("data")
    def * = id ~ data
  }

  val u1 = Some(java.util.UUID.randomUUID())
  val u2 = None
  val u3 = java.util.UUID.randomUUID()
  val u4 = java.util.UUID.randomUUID()
  val u5 = java.util.UUID.randomUUID()

  @Test def testUUIDOption() {
    // A Blob result does not survive a commit on all DBMSs so we wrap everything in a transaction
    db withTransaction {
      T1.ddl.create;
      T1 insert (1, u1)
      T1 insert (2, u2)

      assertEquals(Set((1,u1), (2,u2)),
        Query(T1).list.toSet)
    }
  }

  @Test def testUUID() {
    db withTransaction {
      T2.ddl.create;
      T2.insert (1, u3)
      T2.insert (2, u4)
      assertEquals(Set((1,u3), (2,u4)),
        Query(T2).list.toSet)
    }
  }

  @Test def testMutate() {
    db withTransaction {
      T2.ddl.create;
      T2.insert (1, u3)
      T2.insert (2, u4)
      val q = for {
        t <- T2
      } yield t
      q.mutate { m => if(m.row._1 == 1) m.row = m.row.copy(_2 = u5) }

      assertEquals(Set((1,u5), (2,u4)),
        Query(T2).list.toSet)
    }
  }

  @Test def testUpdate() {
    db withTransaction {
      T2.ddl.create;
      T2.insert (1, u3)
      T2.insert (2, u4)

      val q2 = for { t <- T2 if t.id === 1 } yield t.data
      q2.update(u5)

      assertEquals(Set((1,u5), (2,u4)),
        Query(T2).list.toSet)
    }
  }
}
