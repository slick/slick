package slick.jdbc

sealed trait RowsPerStatement

object RowsPerStatement {
  /** A single statement with all rows should be used */
  case object All extends RowsPerStatement

  /** A separate statement is used for each row */
  case object One extends RowsPerStatement
}
