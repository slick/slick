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

  def testMappedType2 {
      sealed trait EnumType
      case object EnumValue1 extends EnumType
      case object EnumValue2 extends EnumType
      case object EnumValue3 extends EnumType

      implicit val enumTypeMapper = MappedColumnType.base[EnumType, Char](
        { t =>
          assertNotNull(t)
          t match {
            case EnumValue1 => 'A'
            case EnumValue2 => 'B'
            case _ => 'C'
          }
        }, { c =>
          assertNotNull(c)
          c match {
            case 'A' => EnumValue1
            case 'B' => EnumValue2
            case _ => EnumValue3
          }
        }
      )

      class T(tag: Tag) extends Table[(Int, EnumType, Option[EnumType])](tag, "t32") {
        def id = column[Int]("id", O.PrimaryKey, O.AutoInc)
        def b = column[EnumType]("b")
        def c = column[Option[EnumType]]("c")
        def * = (id, b, c)
      }
      val ts = TableQuery[T]

      ts.ddl.create
      ts.map(t => (t.b, t.c)) ++= Seq((EnumValue1, None), (EnumValue1, Some(EnumValue2)), (EnumValue2, Some(EnumValue3)))
      assertEquals(ts.run.toSet, Set((1, EnumValue1, None), (2, EnumValue1, Some(EnumValue2)), (3, EnumValue2, Some(EnumValue3))))
      assertEquals(ts.filter(_.b === (EnumValue1:EnumType)).run.toSet, Set((1, EnumValue1, None), (2, EnumValue1, Some(EnumValue2))))
      assertEquals(ts.filter(_.b === (EnumValue2:EnumType)).run.toSet, Set((3, EnumValue2, Some(EnumValue3))))
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
}

case class MyMappedID(value: Int) extends AnyVal with scala.slick.lifted.MappedTo[Int]
