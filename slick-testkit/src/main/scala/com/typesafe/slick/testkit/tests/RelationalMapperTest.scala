package com.typesafe.slick.testkit.tests

import org.junit.Assert._

import com.typesafe.slick.testkit.util.{RelationalTestDB, TestkitTest}

class RelationalMapperTest extends TestkitTest[RelationalTestDB] {
  import tdb.profile.simple._

  override val reuseInstance = true

  def testMappedType {
    sealed trait Bool
    case object True extends Bool
    case object False extends Bool

    implicit val boolTypeMapper = MappedColumnType.base[Bool, Int](
      { b =>
        assertNotNull(b)
        if(b == True) 1 else 0
      }, { i =>
        assertNotNull(i)
        if(i == 1) True else False
      }
    )

    class T(tag: Tag) extends Table[(Int, Bool, Option[Bool])](tag, "t2") {
      def id = column[Int]("id", O.PrimaryKey, O.AutoInc)
      def b = column[Bool]("b")
      def c = column[Option[Bool]]("c")
      def * = (id, b, c)
    }
    val ts = TableQuery[T]

    ts.ddl.create
    ts.map(t => (t.b, t.c)) ++= Seq((False, None), (True, Some(True)))
    assertEquals(ts.run.toSet, Set((1, False, None), (2, True, Some(True))))
    assertEquals(ts.filter(_.b === (True:Bool)).run.toSet, Set((2, True, Some(True))))
    assertEquals(ts.filter(_.b === (False:Bool)).run.toSet, Set((1, False, None)))
  }

  def testMappedRefType {
    sealed trait Bool
    case object True extends Bool
    case object False extends Bool

    implicit val boolTypeMapper = MappedColumnType.base[Bool, String](
      { b =>
        assertNotNull(b)
        if(b == True) "y" else "n"
      }, { i =>
        assertNotNull(i)
        if(i == "y") True else False
      }
    )

    class T(tag: Tag) extends Table[(Int, Bool, Option[Bool])](tag, "t3") {
      def id = column[Int]("id", O.PrimaryKey, O.AutoInc)
      def b = column[Bool]("b")
      def c = column[Option[Bool]]("c")
      def * = (id, b, c)
    }
    val ts = TableQuery[T]

    ts.ddl.create
    ts.map(t => (t.b, t.c)) ++= Seq((False, None), (True, Some(True)))
    assertEquals(ts.run.toSet, Set((1, False, None), (2, True, Some(True))))
    assertEquals(ts.where(_.b === (True:Bool)).run.toSet, Set((2, True, Some(True))))
    assertEquals(ts.where(_.b === (False:Bool)).run.toSet, Set((1, False, None)))
  }

  def testAutoMapped {

    class T(tag: Tag) extends Table[(MyMappedID, Int)](tag, "t_automapped") {
      def id = column[MyMappedID]("id", O.PrimaryKey)
      def v = column[Int]("v")
      def * = (id, v)
    }
    val ts = TableQuery[T]
    ts.ddl.create
    ts ++= Seq((MyMappedID(1), 2), (MyMappedID(3), 4))

    assertEquals(Set((MyMappedID(1), 2), (MyMappedID(3), 4)), ts.run.toSet)
    assertEquals(Set((MyMappedID(1), 2)), ts.filter(_.id === MyMappedID(1)).run.toSet)
  }

  def mappedToMacroCompilerBug {
    case class MyId(val value: Int) extends MappedTo[Int]

    class MyTable(tag: Tag) extends Table[MyId](tag, "table") {
      def * = ???
    }

    implicitly[Shape[_ <: ShapeLevel.Flat, MyTable, _, _]]
    TableQuery(new MyTable(_)).map(identity)
  }
}

case class MyMappedID(value: Int) extends AnyVal with scala.slick.lifted.MappedTo[Int]
