package slick.dbio

import slick.jdbc.H2Profile.api.*

class ProfileActionExplicitMonadicInferenceTest extends BaseTest {
  test("profile actions work when using explicit cast") {
    monad(ts.result.monadic)
    monad(ts.result.transactionally.monadic)
    monad(ts.schema.create.monadic)
    monad((ts += 1).monadic)
    monad(ts.update(1).monadic)
    monad(ts.delete.monadic)
  }
}

