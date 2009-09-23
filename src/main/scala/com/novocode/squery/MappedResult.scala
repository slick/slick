package com.novocode.squery

import com.novocode.squery.session.PositionedResult

trait MappedResult[R] {

  protected def extractValue(rs: PositionedResult): R

  def close(): Unit
}
