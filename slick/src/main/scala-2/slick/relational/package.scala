package slick
/** Contains the abstract `RelationalProfile` and related code. */
package object relational {

  /** The domain of a `ResultConverter` and associated classes. It defines the
   * `Reader`, `Writer` and `Updater` types that are needed at the lowest
   * level of ResultConverters for accessing the underlying profile-specific
   * data structures. */
  type ResultConverterDomain = {
    type Reader
    type Writer
    type Updater
  }

  type ResultConverterDomainImpl[R, W, U] = ResultConverterDomain {
    type Reader = R
    type Writer = W
    type Updater = U
  }

  type ResultConverterDomainReader[X <: ResultConverterDomain] = X#Reader
  type ResultConverterDomainWriter[X <: ResultConverterDomain] = X#Writer
  type ResultConverterDomainUpdater[X <: ResultConverterDomain] = X#Updater
}
