package com.novocode.squery

trait ResultSetMutator[T] {

  /**
   * Get the current row's value.
   */
  def apply(): T

  /**
   * Update the current row.
   */
  def update(value: T)

  /**
   * Insert a new row.
   */
  def insert(value: T)

  /**
   * Delete the current row.
   */
  def delete(): Unit
}
