package com.typesafe.slick.docs.test

@deprecated("Testing the deprecated Direct Embedding API", "3.0")
class DirectEmbeddingTest extends RecordedDoctest {
  def run = com.typesafe.slick.docs.DirectEmbedding.main(null)
}
