package slick.collection.heterogeneous

/** Extra syntax for heterogenous collections. */
object syntax {
  // Use :: for types and extractors
  type :: [+H, +T <: HList] = HCons[H, T]
  val :: = HCons

  type Zero = slick.collection.heterogeneous.Zero.type
  type HNil = slick.collection.heterogeneous.HNil.type
}
