package slick.dbio

/** A phantom type for annotating DBIOActions with specific effects (e.g. `Write` or
  * `Transactional`). Effects can be composed through intersection types (e.g.
  * `Write with Transactional`. The standard Slick back-ends do not restrict the evaluation of
  * actions based on effects but they can be used in user-level code (e.g. for ensuring that all
  * writes go to a master database but reads can also be performed by a slave). */
trait Effect

object Effect {
  /** Effect for DBIOActions that read from the database ("DQL") */
  trait Read extends Effect
  /** Effect for DBIOActions that write to the database ("DML") */
  trait Write extends Effect
  /** Effect for DBIOActions that manipulate a database schema ("DDL") */
  trait Schema extends Effect
  /** Effect for transactional DBIOActions ("DTL") */
  trait Transactional extends Effect

  /** The bottom type of all standard effects. It is used by the `DBIO` and `StreamingDBIO`
    * type aliases instead of `Nothing` because the compiler does not properly infer `Nothing`
    * where needed. You can still introduce your own custom effect types but they will not be
    * used by `DBIO` and `StreamingDBIO`, so you either have to define your own type aliases
    * or spell out the proper `DBIOAction` types in type annotations. */
  trait All extends Read with Write with Schema with Transactional
}
