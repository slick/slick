package slick.compiler

/** Infer all missing types. */
class InferTypes extends Phase {
  val name = "inferTypes"

  def apply(state: CompilerState) = state.map(_.infer(typeChildren = true))
}
