package scala.slick

package object action {
  type StreamingAction[+R, +T] = EffectfulAction[Effect.All, R, Streaming[T]]

  type Action[+R] = EffectfulAction[Effect.All, R, NoStream]
}
