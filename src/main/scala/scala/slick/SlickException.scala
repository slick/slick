package scala.slick

/** All Exceptions that are thrown directly by Slick are of type `SlickException`.
  * Other Exceptions originating in non-Slick code are generally not wrapped but
  * passed on directly.

  * @param msg The error message
  * @param parent An optional parent Exception or `null`
  */
class SlickException(msg: String, parent: Throwable = null) extends RuntimeException(msg, parent)
