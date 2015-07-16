package scala

import scala.language.higherKinds

package object slick {
  @deprecated("Package 'scala.slick' has been renamed to 'slick'", "3.0")
  type SlickException = _root_.slick.SlickException

  @deprecated("Package 'scala.slick' has been renamed to 'slick'", "3.0")
  object backend {
    type DatabaseComponent = _root_.slick.backend.DatabaseComponent
  }

  object collection {
    @deprecated("Package 'scala.slick.collection.heterogenous' has been renamed to 'slick.collection.heterogeneous'", "3.0")
    object heterogenous {
      type HList = _root_.slick.collection.heterogeneous.HList
      val HList = _root_.slick.collection.heterogeneous.HList
      val HNil = _root_.slick.collection.heterogeneous.HNil
      type HCons[+H, +T <: HList] = _root_.slick.collection.heterogeneous.HCons[H, T]
      val HCons = _root_.slick.collection.heterogeneous.HCons
      val syntax = _root_.slick.collection.heterogeneous.syntax
      type TypedFunction = _root_.slick.collection.heterogeneous.TypedFunction
      type TypedFunction2[-T1, -T2, +TR, F[_ <: T1, _ <: T2] <: TR] = _root_.slick.collection.heterogeneous.TypedFunction2[T1, T2, TR, F]
      type Nat = _root_.slick.collection.heterogeneous.Nat
      val Nat = _root_.slick.collection.heterogeneous.Nat
      type Succ[N <: Nat] = _root_.slick.collection.heterogeneous.Succ[N]
      val Zero = _root_.slick.collection.heterogeneous.Zero
    }
  }

  @deprecated("Package 'scala.slick' has been renamed to 'slick'", "3.0")
  object driver {
    type JdbcProfile = _root_.slick.driver.JdbcProfile
    type DerbyDriver = _root_.slick.driver.DerbyDriver
    type H2Driver = _root_.slick.driver.H2Driver
    type HsqldbDriver = _root_.slick.driver.HsqldbDriver
    type JdbcDriver = _root_.slick.driver.JdbcDriver
    type MySQLDriver = _root_.slick.driver.MySQLDriver
    type PostgresDriver = _root_.slick.driver.PostgresDriver
    type SQLiteDriver = _root_.slick.driver.SQLiteDriver
    val DerbyDriver = _root_.slick.driver.DerbyDriver
    val H2Driver = _root_.slick.driver.H2Driver
    val HsqldbDriver = _root_.slick.driver.HsqldbDriver
    val JdbcDriver = _root_.slick.driver.JdbcDriver
    val MySQLDriver = _root_.slick.driver.MySQLDriver
    val PostgresDriver = _root_.slick.driver.PostgresDriver
    val SQLiteDriver = _root_.slick.driver.SQLiteDriver
  }

  @deprecated("Package 'scala.slick' has been renamed to 'slick'", "3.0")
  object jdbc {
    type StaticQuery[-P,+R] = _root_.slick.jdbc.StaticQuery[P, R]
    val StaticQuery = _root_.slick.jdbc.StaticQuery
    type JdbcBackend = _root_.slick.jdbc.JdbcBackend
    val JdbcBackend = _root_.slick.jdbc.JdbcBackend
  }

  @deprecated("Package 'scala.slick' has been renamed to 'slick'", "3.0")
  object memory {
    type MemoryProfile = _root_.slick.memory.MemoryProfile
    type MemoryDriver = _root_.slick.memory.MemoryDriver
    val MemoryDriver = _root_.slick.memory.MemoryDriver
    type HeapBackend = _root_.slick.memory.HeapBackend
    val HeapBackend = _root_.slick.memory.HeapBackend
    type DistributedBackend = _root_.slick.memory.DistributedBackend
    val DistributedBackend = _root_.slick.memory.DistributedBackend
  }

  @deprecated("Package 'scala.slick' has been renamed to 'slick'", "3.0")
  object profile {
    type BasicProfile = _root_.slick.profile.BasicProfile
    type RelationalProfile = _root_.slick.profile.RelationalProfile
    type SqlProfile = _root_.slick.profile.SqlProfile
    val RelationalProfile = _root_.slick.profile.RelationalProfile
    val SqlProfile = _root_.slick.profile.SqlProfile
  }
}
