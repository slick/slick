package com.novocode.squery.combinator.sql

import scala.collection.mutable.HashMap
import java.io.PrintWriter
import com.novocode.squery.combinator._

class SequenceDDLBuilder(seq: Sequence[_]) {

  def buildCreateSequence = {
    val b = new StringBuilder append "CREATE SEQUENCE " append seq.name
    seq._increment.foreach { b append " INCREMENT " append _ }
    seq._minValue.foreach { b append " MINVALUE " append _ }
    seq._maxValue.foreach { b append " MAXVALUE " append _ }
    seq._start.foreach { b append " START " append _ }
    if(seq._cycle) b append " CYCLE"
    b toString
  }
}
