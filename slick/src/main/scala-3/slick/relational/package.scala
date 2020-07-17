package slick
/** Contains the abstract `RelationalProfile` and related code. */
package object relational {

  /** The domain of a `ResultConverter` and associated classes. It defines the
   * `Reader`, `Writer` and `Updater` types that are needed at the lowest
   * level of ResultConverters for accessing the underlying profile-specific
   * data structures. */
  type ResultConverterDomain

  type ResultConverterDomainImpl[Reader, Writer, Updater] <: ResultConverterDomain

  type ResultConverterDomainReader[X] = X match {
    case ResultConverterDomainImpl[t, _, _] => t
  }

  type ResultConverterDomainWriter[X] = X match {
    case ResultConverterDomainImpl[_, t, _] => t
  }

  type ResultConverterDomainUpdater[X] = X match {
    case ResultConverterDomainImpl[_, _, t] => t
  }
}
