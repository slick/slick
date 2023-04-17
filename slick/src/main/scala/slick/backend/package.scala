package slick

import slick.basic.BasicProfile

package object backend {
  @deprecated("Use type `slick.basic.BasicBackend` instead of `slick.backend.DatabaseComponent`", "3.2")
  type DatabaseComponent = slick.basic.BasicBackend

  @deprecated("Use type `slick.basic.DatabasePublisher` instead of `slick.backend.DatabasePublisher`", "3.2")
  type DatabasePublisher[T] = slick.basic.DatabasePublisher[T]

  @deprecated("Use type `slick.relational.RelationalBackend` instead of `slick.backend.RelationalBackend`", "3.2")
  type RelationalBackend = slick.relational.RelationalBackend

  @deprecated("Use type `slick.basic.DatabaseConfig` instead of `slick.backend.DatabaseConfig`", "3.2")
  type DatabaseConfig[P <: BasicProfile] = slick.basic.DatabaseConfig[P]
  @deprecated("Use object `slick.basic.DatabaseConfig` instead of `slick.backend.DatabaseConfig`", "3.2")
  val DatabaseConfig = slick.basic.DatabaseConfig
}
