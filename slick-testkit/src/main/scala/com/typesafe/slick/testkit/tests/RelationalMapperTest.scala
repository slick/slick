package com.typesafe.slick.testkit.tests

import scala.language.existentials

import com.typesafe.slick.testkit.util.{RelationalTestDB, AsyncTest}

class RelationalMapperTest extends AsyncTest[RelationalTestDB] {
  import tdb.profile.api._

  def testMappedType = {
    sealed trait Bool
    case object True extends Bool
    case object False extends Bool

    implicit val boolTypeMapper = MappedColumnType.base[Bool, Int](
      { b =>
        b shouldNotBe null
        if(b == True) 1 else 0
      }, { i =>
        i shouldNotBe null
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

    seq(
      ts.schema.create,
      ts.map(t => (t.b, t.c)) ++= Seq((False, None), (True, Some(True))),
      ts.to[Set].result.map(_ shouldBe Set((1, False, None), (2, True, Some(True)))),
      ts.filter(_.b === (True:Bool)).to[Set].result.map(_ shouldBe Set((2, True, Some(True)))),
      ts.filter(_.b === (False:Bool)).to[Set].result.map(_ shouldBe Set((1, False, None)))
    )
  }

  def testMappedType2 = {
    sealed trait EnumType
    case object EnumValue1 extends EnumType
    case object EnumValue2 extends EnumType
    case object EnumValue3 extends EnumType

    implicit val enumTypeMapper = MappedColumnType.base[EnumType, Char](
      { t =>
        t shouldNotBe null
        t match {
          case EnumValue1 => 'A'
          case EnumValue2 => 'B'
          case _ => 'C'
        }
      }, { c =>
        c shouldNotBe null
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

    seq(
      ts.schema.create,
      ts.map(t => (t.b, t.c)) ++= Seq((EnumValue1, None), (EnumValue1, Some(EnumValue2)), (EnumValue2, Some(EnumValue3))),
      ts.to[Set].result.map(_ shouldBe Set((1, EnumValue1, None), (2, EnumValue1, Some(EnumValue2)), (3, EnumValue2, Some(EnumValue3)))),
      ts.filter(_.b === (EnumValue1:EnumType)).to[Set].result.map(_ shouldBe Set((1, EnumValue1, None), (2, EnumValue1, Some(EnumValue2)))),
      ts.filter(_.b === (EnumValue2:EnumType)).to[Set].result.map(_ shouldBe Set((3, EnumValue2, Some(EnumValue3))))
    )
  }

  def testMappedRefType = {
    sealed trait Bool
    case object True extends Bool
    case object False extends Bool

    implicit val boolTypeMapper = MappedColumnType.base[Bool, String](
      { b =>
        b shouldNotBe null
        if(b == True) "y" else "n"
      }, { i =>
        i shouldNotBe null
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

    seq(
      ts.schema.create,
      ts.map(t => (t.b, t.c)) ++= Seq((False, None), (True, Some(True))),
      ts.to[Set].result.map(_ shouldBe Set((1, False, None), (2, True, Some(True)))),
      ts.filter(_.b === (True:Bool)).to[Set].result.map(_ shouldBe Set((2, True, Some(True)))),
      ts.filter(_.b === (False:Bool)).to[Set].result.map(_ shouldBe Set((1, False, None)))
    )
  }

  def testAutoMapped = {

    class T(tag: Tag) extends Table[(MyMappedID, Int)](tag, "t_automapped") {
      def id = column[MyMappedID]("id", O.PrimaryKey)
      def v = column[Int]("v")
      def * = (id, v)
    }
    val ts = TableQuery[T]

    seq(
      ts.schema.create,
      ts ++= Seq((MyMappedID(1), 2), (MyMappedID(3), 4)),
      ts.to[Set].result.map(_ shouldBe Set((MyMappedID(1), 2), (MyMappedID(3), 4))),
      ts.filter(_.id === MyMappedID(1)).to[Set].result.map(_ shouldBe Set((MyMappedID(1), 2)))
    )
  }

  def mappedToMacroCompilerBug = {
    case class MyId(val value: Int) extends MappedTo[Int]

    class MyTable(tag: Tag) extends Table[MyId](tag, "table") {
      def * = ???
    }

    implicitly[Shape[_ <: FlatShapeLevel, MyTable, _, _]]
    TableQuery(new MyTable(_)).map(identity)
  }
}

case class MyMappedID(value: Int) extends AnyVal with slick.lifted.MappedTo[Int]
