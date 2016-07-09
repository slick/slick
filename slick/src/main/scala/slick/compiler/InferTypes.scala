package slick.compiler

/** Infer all missing types. */
class InferTypes extends Phase {
  val name = "inferTypes"

  def apply(state: CompilerState) = state.map{n =>
    val n2 = n.infer(typeChildren = true)
    logger.debug("After inferring types", n2)
    n2
  }
}
