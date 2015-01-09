package scala.slick

package object action {
  type StreamingAction[+R, +T] = EffectfulAction[Nothing, R, Streaming[T]]

  type Action[+R] = EffectfulAction[Nothing, R, NoStream]
}
