package com.typesafe.slick.docs.test

import slick.testutil.RecordedTest

abstract class RecordedDoctest extends RecordedTest {

  def basename = "slick-testkit/src/doctest/resources/" + getClass.getName.replaceAll("^.*\\.", "")

  override def mask(line: String) =
    if(line.startsWith("select ")) "select..." else line
}
