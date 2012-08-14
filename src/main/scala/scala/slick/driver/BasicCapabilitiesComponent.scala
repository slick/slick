package scala.slick.driver

/** Describes the limits of the driver with regards to the profile. */

trait BasicCapabilitiesComponent { driver: BasicDriver =>
  val capabilities = new Capabilities

  class Capabilities {
    /** Supports the Blob data type */
    val blob = true
    /** Supports sequences (real or emulated) */
    val sequence = true
    /** Can get current sequence value */
    val currval = true
    /** Supports zip, zipWith and zipWithIndex */
    val zip = true
  }
}
