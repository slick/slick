package slick.dbio

import slick.jdbc.H2Profile.api.*

class ProfileActionInferenceTest extends BaseTest {
  test("non-streaming profile actions do not require explicit cast on scala3 when source:3.0-migration is off") {
    monad(ts.result.map(_ => 1))
    monad(ts += 1)
    monad(ts.schema.create)
    monad(ts.update(1))
    monad(ts.delete: DBIOAction[Int, NoStream, Effect.Write])
  }
}

