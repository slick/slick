package org.scalaquery.session

/**
 * Describes capabilities of the database which can be determined from a
 * DatabaseMetaData object and then cached and reused for all sessions.
 */
class DatabaseCapabilities(session: Session) {
  val supportsBatchUpdates = session.metaData.supportsBatchUpdates
}
