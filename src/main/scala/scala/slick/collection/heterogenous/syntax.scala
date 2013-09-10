package scala.slick.collection.heterogenous

/** Extra syntax for heterogenous collections. */
object syntax {
  // Use :: for types and extractors
  type :: [+H, +T <: HList] = HCons[H, T]
  val :: = HCons

  type Zero = scala.slick.collection.heterogenous.Zero.type
  type HNil = scala.slick.collection.heterogenous.HNil.type
}
