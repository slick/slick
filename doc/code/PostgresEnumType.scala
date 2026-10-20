package com.typesafe.slick.docs

import java.sql.{PreparedStatement, Types}

import slick.ast.{BaseTypedType, FieldSymbol}
import slick.jdbc.PostgresProfile

object PostgresEnumType {
  import PostgresProfile.api.*

  //#postgresEnum
  // A Scala enumeration stored in a PostgreSQL enum column, created with
  // create type mood as enum ('Happy', 'Sad')
  object Mood extends Enumeration {
    val Happy, Sad = Value
  }

  implicit val moodColumnType: BaseColumnType[Mood.Value] =
    new PostgresProfile.MappedJdbcType[Mood.Value, String] with BaseTypedType[Mood.Value] {
      def map(m: Mood.Value) = m.toString
      def comap(s: String) = Mood.withName(s)

      // The database type, for DDL and for a None literal: cast(null as mood)
      override def newSqlTypeName(sym: Option[FieldSymbol]) = Some("mood")

      // Send values untyped, so that PostgreSQL converts them to the enum
      override def setValue(m: Mood.Value, p: PreparedStatement, idx: Int) =
        p.setObject(idx, map(m), Types.OTHER)
      override def setNull(p: PreparedStatement, idx: Int) = p.setNull(idx, Types.OTHER)
    }
  //#postgresEnum

  //#postgresEnumCast
  // A table whose mood column is mapped as a plain String
  class People(tag: Tag) extends Table[(Int, String)](tag, "people") {
    def id = column[Int]("id")
    def mood = column[String]("mood")
    def * = (id, mood)
  }
  val people = TableQuery[People]

  // Written as cast(null as mood) instead of cast(null as VARCHAR)
  val noMood = LiteralColumn(Option.empty[String]).asColumnOfType[Option[String]]("mood")
  val moods = people.map(p => (p.id, p.mood.?)).union(people.map(p => (p.id + 100, noMood)))
  //#postgresEnumCast
}
