package slick.dbio

import cats.syntax.all.*

// check that instances are picked up when using either DBIO or SlickAction while preserving Effect type
class SlickActionInferenceTest extends BaseTest {
  test("the slick-cats 'Known Issues' cases compile") {
    // SlickAction replaces DBIOAction
    val fail1: SlickAction[NoStream, Effect.All, String] = DBIO.successful("hello")
    val fail2 = DBIO.successful("hello")
    val success: DBIO[String] = DBIO.successful("hello")

    monad(fail1)
    monad(fail2)
    monad(success)
  }

  val action = DBIO.successful("hello")
  val io: DBIO[String] = action

  test("specific effect is preserved") {
    exactly[SlickAction[NoStream, Effect.Transactional, String]] {
      monad(action.flatMap(_ => action.transactionally))
    }
  }

  test("traverse picks up Monad") {
    exactly[SlickAction[NoStream, Effect, List[String]]] {
      List(1).traverse { i => action }
    }
    exactly[DBIO[List[String]]] {
      List(1).traverse { i => io }
    }
  }

  test("cats syntax works") {
    action >>= (_ => action)
    io >>= (_ => action)
    action |+| action
    io |+| action
  }
}

