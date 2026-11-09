package com.typesafe.slick.testkit.tests

import com.typesafe.slick.testkit.util.{AsyncTest, RelationalTestDB}

class RelationalMapperTest extends AsyncTest[RelationalTestDB] {
  import tdb.profile.api.*

  def testMappedType = {
    sealed trait Bool
    case object True extends Bool
    case object False extends Bool

    implicit val boolTypeMapper: BaseColumnType[Bool] = MappedColumnType.base[Bool, Int](
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

    implicit val enumTypeMapper: BaseColumnType[EnumType] = MappedColumnType.base[EnumType, Char](
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

    implicit val boolTypeMapper: BaseColumnType[Bool] = MappedColumnType.base[Bool, String](
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

  def testMappedTypeInSubquery842 = {
    // Regression test for issue #842: a MappedColumnType column referenced via
    // `column[T](name)` inside an IN subquery filter used to apply the type mapping
    // in the wrong place, causing a ClassCastException at execution. After the fix
    // the query runs and returns the correct rows.
    case class Unfetched(value: Long)

    implicit val unfetchedType: BaseColumnType[Unfetched] =
      MappedColumnType.base[Unfetched, Long](_.value - 1000, db => Unfetched(db + 1000))

    case class Person(id: Long, name: String)

    class People(tag: Tag) extends Table[Person](tag, "people842") {
      def id = column[Long]("id", O.PrimaryKey)
      def name = column[String]("name")
      def * = (id, name).mapTo[Person]
    }
    val people = TableQuery[People]

    class Phones(tag: Tag) extends Table[(Long, Long)](tag, "phones842") {
      def num = column[Long]("num", O.PrimaryKey)
      def person = column[Long]("person")
      def * = (num, person)
    }
    val phones = TableQuery[Phones]

    // Carol (id 3) has no phone, so the IN-subquery filter must exclude her.
    val q = people.filter(_.column[Unfetched]("id").in(phones.map(_.column[Unfetched]("person"))))

    seq(
      (people.schema ++ phones.schema).create,
      people ++= Seq(Person(1L, "Alice"), Person(2L, "Bob"), Person(3L, "Carol")),
      phones ++= Seq((100L, 1L), (101L, 2L)),
      q.to[Set].result.map(_ shouldBe Set(Person(1L, "Alice"), Person(2L, "Bob")))
    )
  }

  private case class Index(value: Int)

  private class MinusOneAsNoneTable(tag: Tag, name: String) extends Table[Option[Index]](tag, name) {
    import MinusOneAsNoneTable.*
    def value = column[Option[Index]]("value")
    def * = value
  }
  private object MinusOneAsNoneTable {
    /** Maps a whole Option[Index] to a NOT NULL integer, with -1 standing for None (like indexOf) */
    implicit val minusOneAsNone: BaseColumnType[Option[Index]] =
      MappedColumnType.base[Option[Index], Int](_.fold(-1)(_.value), { case -1 => None; case i => Some(Index(i)) })
  }

  def testOptionMappedAsWholeReadsNone = {
    val ts = TableQuery(new MinusOneAsNoneTable(_, "reads_none"))
    seq(ts.schema.create, ts += None, ts.result.map(_ shouldBe Seq(None)))
  }

  def testOptionMappedAsWholeReadsSome = {
    val ts = TableQuery(new MinusOneAsNoneTable(_, "reads_some"))
    seq(ts.schema.create, ts += Some(Index(1)), ts.result.map(_ shouldBe Seq(Some(Index(1)))))
  }

  /** A None literal of the mapped type is the value -1, not NULL */
  def testOptionMappedAsWholeSelectsNoneLiteral = {
    import MinusOneAsNoneTable.*
    Query(LiteralColumn(Option.empty[Index])).result.map(_ shouldBe Seq(None))
  }

  /** Inside an optional tuple, a None literal of the mapped type is still the value -1, not NULL */
  def testOptionMappedAsWholeNoneLiteralInOptionalTuple = {
    import MinusOneAsNoneTable.*
    val ts = TableQuery(new MinusOneAsNoneTable(_, "none_literal_in_optional_tuple"))
    val q = ts.joinLeft(ts).on(_.value === _.value).map { case (_, r) => r.map(r => (r.value, LiteralColumn(Option.empty[Index]))) }
    seq(ts.schema.create, ts += Some(Index(1)), q.result.map(_ shouldBe Seq(Some((Some(Index(1)), None)))))
  }

  /** The column is not nullable although its Scala type is an Option, so None compares equal to -1 */
  def testOptionMappedAsWholeComparedWithOption = {
    import MinusOneAsNoneTable.*
    val ts = TableQuery(new MinusOneAsNoneTable(_, "compared_with_option"))
    def matching(value: Option[Index]) = ts.filter(_.value === value)
    seq(
      ts.schema.create,
      ts ++= Seq(None, Some(Index(1)), Some(Index(2))),
      matching(Some(Index(2))).result.map(_ shouldBe Seq(Some(Index(2)))),
      matching(None).result.map(_ shouldBe Seq(None)),
      ts.filter(_.value =!= Option(Index(1))).to[Set].result.map(_ shouldBe Set(None, Some(Index(2))))
    )
  }

  /** In Case.If, a None of the mapped type is the value -1, not NULL, and the result keeps the mapped type */
  def testOptionMappedAsWholeInCase = {
    import MinusOneAsNoneTable.*
    val ts = TableQuery(new MinusOneAsNoneTable(_, "in_case"))
    // A missing index counts as Index(0)
    val effective =
      ts.map(t => Case.If(t.value === Option.empty[Index]).Then(LiteralColumn(Option(Index(0)))).Else(t.value))
    seq(
      ts.schema.create,
      ts ++= Seq(None, Some(Index(1)), Some(Index(2))),
      effective.to[Set].result.map(_ shouldBe Set(Some(Index(0)), Some(Index(1)), Some(Index(2))))
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
      ts.to[Set]
        .result
        .map(_ shouldBe Set((MyMappedID(1), 2), (MyMappedID(3), 4))),
      ts.filter(_.id === MyMappedID(1))
        .to[Set]
        .result
        .map(_ shouldBe Set((MyMappedID(1), 2)))
    )
  }

  def mappedToMacroCompilerBug = {
    case class MyId(val value: Int) extends MappedTo[Int]

    class MyTable(tag: Tag) extends Table[MyId](tag, "table") {
      def * : slick.lifted.ProvenShape[MyId] = ???
    }

    implicitly[Shape[_ <: FlatShapeLevel, MyTable, _, _]]
    TableQuery(new MyTable(_)).map(identity)
  }

  def testMappedProjectionShape = {
    def toLower(s: Option[String]) = s.map(_.toLowerCase)
    def toUpper(s: Option[String]): Option[Option[String]] = Some(
      s.map(_.toUpperCase)
    )

    case class Row(id: String, escaped: Option[String])

    class T(tag: Tag) extends Table[Row](tag, "t".withUniquePostFix) {
      val id = column[String]("id", O.PrimaryKey)
      val name = column[Option[String]]("name")
      val upperName = name.shaped.<>(toLower, toUpper)
      def * = (id, upperName).<>(
        (a: (String, Option[String])) => Row(a._1, a._2),
        Row.unapply
      )
    }
    val ts = TableQuery[T]

    seq(
      ts.schema.create,
      ts += Row("a", Some("foo")),
      ts.map(_.name).result.map(_ shouldBe Seq(Some("FOO"))),
      ts.result.map(_ shouldBe Seq(Row("a", Some("foo"))))
    )
  }
}

case class MyMappedID(value: Int) extends AnyVal with slick.lifted.MappedTo[Int]
