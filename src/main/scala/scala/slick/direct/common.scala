package scala.slick.direct
object common{
  def SLICK_ONLY : Nothing = throw new Exception("This method can only be used in queries translated by Slick.")
  def SLICK_INTERNAL : Nothing = throw new Exception("This method is Slick internal and cannot be used alone.")
}