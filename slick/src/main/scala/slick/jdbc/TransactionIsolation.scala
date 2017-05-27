package slick.jdbc

import java.sql.Connection

/** Represents a transaction isolation level. */
sealed abstract class TransactionIsolation(val intValue: Int)

object TransactionIsolation {
  /** A transaction isolation level indicating that dirty reads, non-repeatable reads and phantom
    * reads can occur. This level allows a row changed by one transaction to be readby another
    * transaction before any changes in that row have been committed (a "dirty read"). If any of
    * the changes are rolled back, the second transaction will have retrieved an invalid row. */
  case object ReadUncommitted extends TransactionIsolation(Connection.TRANSACTION_READ_UNCOMMITTED)

  /** A transaction isolation level indicating that dirty reads are prevented; non-repeatable reads
    * and phantom reads can occur. This level only prohibits a transaction from reading a row with
    * uncommitted changes in it. */
  case object ReadCommitted extends TransactionIsolation(Connection.TRANSACTION_READ_COMMITTED)

  /** A transaction isolation level indicating that dirty reads and non-repeatable reads are
    * prevented; phantom reads can occur. This level prohibits a transaction from reading a row
    * with uncommitted changes in it, and it also prohibits the situation where one transaction
    * reads a row, a second transaction alters the row, and the first transaction rereads the row,
    * getting different values the second time (a "non-repeatable read"). */
  case object RepeatableRead extends TransactionIsolation(Connection.TRANSACTION_REPEATABLE_READ)

  /** A transaction isolation level indicating that dirty reads, non-repeatable reads and phantom
    * reads are prevented. This level includes the prohibitions in
    * <code>TRANSACTION_REPEATABLE_READ</code> and further prohibits the situation where one
    * transaction reads all rows that satisfy a <code>WHERE</code> condition, a second transaction
    * inserts a row that satisfies that <code>WHERE</code> condition, and the first transaction
    * rereads for the same condition, retrieving the additional "phantom" row in the second read. */
  case object Serializable extends TransactionIsolation(Connection.TRANSACTION_SERIALIZABLE)
}
