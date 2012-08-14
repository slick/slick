package scala.slick.driver

/** Describes the limits of the driver with regards to the profile. */

trait BasicCapabilitiesComponent { driver: BasicDriver =>
  val capabilities = new Capabilities

  class Capabilities {
    /** Supports the Blob data type */
    val blob = true
    /** Supports default values in column definitions */
    val columnDefaults = true
    /** Supports .drop on queries */
    val pagingDrop = true
    /** Supports mutable result sets */
    val mutable = true
    /** Supports sequences (real or emulated) */
    val sequence = true
    /** Can get current sequence value */
    val currval = true
    /** Supports zip, zipWith and zipWithIndex */
    val zip = true
  }
}
