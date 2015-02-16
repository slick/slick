package slick.dbio

/** A phantom type used as the streaming result type for DBIOActions that do not support streaming.
  * Note that this is a supertype of `Streaming` (and it is used in covariant position),
  * so that any streaming action can be used where a non-streaming action is expected. */
sealed trait NoStream

/** A phantom type used as the streaming result type for DBIOActions that do support streaming. */
sealed trait Streaming[+T] extends NoStream
