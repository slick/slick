package scala.slick.backend

import scala.slick.action.Effect

/** The required backend level for RelationalProfile. */
trait RelationalBackend extends DatabaseComponent {
  type Effects <: Effect.Read with Effect.Write with Effect.Schema with Effect.BackendType[This]
}
