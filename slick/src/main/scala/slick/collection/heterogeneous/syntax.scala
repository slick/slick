package slick.collection.heterogeneous

/** Extra syntax for heterogenous collections. */
object syntax {
  // Use :: for types and extractors
  type :: [+H, +T <: HList] = HCons[H, T]
  val :: = HCons

  type HNil = slick.collection.heterogeneous.HNil.type
}
