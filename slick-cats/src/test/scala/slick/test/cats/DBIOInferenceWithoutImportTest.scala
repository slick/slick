package slick.test.cats

import munit.FunSuite

/** Without the import, nothing resolves: the instances are opt-in. Kept in its own file so
  * that no `slick.cats.dbio.instances` import is in scope for the `compileErrors` snippet. */
class DBIOInferenceWithoutImportTest extends FunSuite {
  test("no instance is found without importing slick.cats.dbio.instances") {
    val errors = compileErrors("""
      import cats.syntax.all.*
      import slick.dbio.*
      def read: DBIOAction[Int, NoStream, Effect.Read] = DBIO.successful(1)
      List(read, read).sequence
    """)
    assert(errors.contains("No given instance"), errors)
  }
}
