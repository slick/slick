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
}
