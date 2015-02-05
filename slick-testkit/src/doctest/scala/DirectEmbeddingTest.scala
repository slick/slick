package com.typesafe.slick.examples.test

@deprecated("Testing the deprecated Direct Embedding API", "3.0")
class DirectEmbeddingTest extends RecordedDoctest {
  def run = com.typesafe.slick.examples.direct.DirectEmbedding.main(null)
}
