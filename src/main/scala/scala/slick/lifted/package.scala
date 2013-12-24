package scala.slick
/** Lifted embedding: Stable query api based on implicits and overloading lifting Scala code into Slick AST's */
package object lifted{
  @deprecated("Use scala.slick.model.ForeignKeyAction instead","2.0.0")
  val  ForeignKeyAction = scala.slick.model.ForeignKeyAction
  @deprecated("Use scala.slick.model.ForeignKeyAction instead","2.0.0")
  type ForeignKeyAction = scala.slick.model.ForeignKeyAction
}
