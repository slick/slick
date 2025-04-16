package slick.lifted


import slick.ast._
import slick.ast.Filter
import slick.model

import slick.jdbc.PostgresProfile.api.*

// TODO: Put in correct place and test the bad cases
object MapToTesting {
  case class Id(i: Int)
  implicit val idMapper: BaseColumnType[Id] = MappedColumnType.base[Id, Int](_.i, Id.apply)

  case class Name(string: String, other: Option[String] = None)
  implicit val nameMapper: BaseColumnType[Name] = MappedColumnType.base[Name, String](_.string, Name(_, None))

  case class IntStringTable(id: Int, string: String)
  case class NameTable(name: Name)
  case class IntTable(int: Int)
  case class IdTable(id: Id)
  case class ComplexTable(id: Id, name: Name, string: String, int: Int, idOpt: Option[Id], nameOpt: Option[Name], stringOpt: Option[String], intOpt: Option[Int])
  case class IntIntIntIntTable(int1: Int, int2: Int, int3: Int, int4: Int)
  case class Table22(
    field1: Int, field2: Int, field3: Int, field4: Int, field5: Int, field6: Int, field7: Int, field8: Int,
    field9: Int, field10: Int, field11: Int, field12: Int, field13: Int, field14: Int, field15: Int, field16: Int,
    field17: Int, field18: Int, field19: Int, field20: Int, field21: Int, field22: Int
  )

  case class Table24(
    field1: Int, field2: Int, field3: Int, field4: Int, field5: Int, field6: Int, field7: Int, field8: Int,
    field9: Int, field10: Int, field11: Int, field12: Int, field13: Int, field14: Int, field15: Int, field16: Int,
    field17: Int, field18: Int, field19: Int, field20: Int, field21: Int, field22: Int, field24: Int, field25: Int,
  )

  import slick.collection.heterogeneous.*

    def id: Rep[Id] = ???
    def idOpt: Rep[Option[Id]] = ???
    def int: Rep[Int] = ???
    def intOpt: Rep[Option[Int]] = ???
    def string: Rep[String] = ???
    def stringOpt: Rep[Option[String]] = ???
    def name: Rep[Name] = ???
    def nameOpt: Rep[Option[Name]] = ???

    int.mapTo[IntTable]
    Tuple(int).mapTo[IntTable]
    (int :: HNil).mapTo[IntTable]

    id.mapTo[IdTable]
    Tuple(id).mapTo[IdTable]
    (id :: HNil).mapTo[IdTable]

    name.mapTo[NameTable]
    Tuple(name).mapTo[NameTable]
    (name :: HNil).mapTo[NameTable]

    (int, string).mapTo[IntStringTable]
    (int :: string :: HNil).mapTo[IntStringTable]

    (int, int, int, int).mapTo[IntIntIntIntTable]
    (int :: int :: int :: int :: HNil).mapTo[IntIntIntIntTable]

    (id, name, string, int, idOpt, nameOpt, stringOpt, intOpt).mapTo[ComplexTable]
    (id :: name :: string :: int :: idOpt :: nameOpt :: stringOpt :: intOpt :: HNil).mapTo[ComplexTable]

    (int :: int :: int :: int :: int :: int :: int :: int :: int :: int :: int :: int :: int :: int :: int :: int :: int :: int :: int :: int :: int :: int :: HNil).mapTo[Table22]
    (int :: int :: int :: int :: int :: int :: int :: int :: int :: int :: int :: int :: int :: int :: int :: int :: int :: int :: int :: int :: int :: int :: int :: int :: HNil).mapTo[Table24]

    // Wrong type:
    // (id :: HNil).mapTo[IntTable]

    // Wrong type:
    // int.mapTo[IdTable]

    // Wrong type:
    // (id :: HNil).mapTo[NameTable]

    // Wrong order:
    // (string :: int :: HNil).mapTo[IntStringTable]

    // Right order, wrong wrappers
    // (idOpt :: nameOpt :: stringOpt :: intOpt :: id :: name :: string :: int :: HNil).mapTo[ComplexTable]

    // Right order, wrong wrappers, tuple
    // (idOpt, nameOpt, stringOpt, intOpt, id, name, string, int).mapTo[ComplexTable]

    // Single error at the end
    // (int :: int :: int :: int :: int :: int :: int :: int :: int :: int :: int :: int :: int :: int :: int :: int :: int :: int :: int :: int :: int :: string :: HNil).mapTo[Table22]

    // Single error in the middle
    // (int, int, int, int, int, int, int, int, int, int, id, int, int, int, int, int, int, int, int, int, int, int).mapTo[Table22]

    // Wrong length
    // (int, int, int, int, int, int).mapTo[IntIntIntIntTable]

    // Wrong length - hlist
    // (int :: int :: int :: int :: int :: int :: HNil).mapTo[IntIntIntIntTable]

    // Wrong length - long
    // (int :: int :: int :: int :: int :: int :: int :: int :: int :: int :: int :: int :: int :: int :: int :: int :: int :: int :: int :: int :: int :: HNil).mapTo[Table22]

    // Wrong type - 22+ hlist
    // (int :: int :: int :: int :: int :: int :: int :: int :: int :: int :: int :: int :: int :: int :: int :: int :: int :: int :: int :: int :: idOpt :: int :: int :: int :: HNil).mapTo[Table24]

}
